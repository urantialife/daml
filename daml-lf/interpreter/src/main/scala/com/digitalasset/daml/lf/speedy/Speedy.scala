// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package speedy

import java.util
import com.daml.lf.data.Ref._
import com.daml.lf.data.{FrontStack, ImmArray, Ref, Time}
import com.daml.lf.interpretation.{Error => IError}
import com.daml.lf.language.Ast._
import com.daml.lf.language.{LookupError, Util => AstUtil}
import com.daml.lf.ledger.Authorize
import com.daml.lf.speedy.Compiler.{CompilationError, PackageNotFound}
import com.daml.lf.speedy.SError._
import com.daml.lf.speedy.SExpr._
import com.daml.lf.speedy.SResult._
import com.daml.lf.speedy.SBuiltin.checkAborted
import com.daml.lf.transaction.{
  ContractKeyUniquenessMode,
  GlobalKey,
  IncompleteTransaction,
  Node,
  TransactionVersion,
}
import com.daml.lf.value.{Value => V}
import com.daml.nameof.NameOf
import com.daml.scalautil.Statement.discard
import com.daml.logging.{ContextualizedLogger, LoggingContext}

import scala.annotation.tailrec
import scala.util.control.NoStackTrace

private[lf] object Speedy {

  // Would like these to have zero cost when not enabled. Better still, to be switchable at runtime.
  private[this] val enableInstrumentation: Boolean = false
  private[this] val enableLightweightStepTracing: Boolean = false

  /** Instrumentation counters. */
  final case class Instrumentation(
      var classifyCounts: Classify.Counts,
      var countPushesKont: Int,
      var countPushesEnv: Int,
      var maxDepthKont: Int,
      var maxDepthEnv: Int,
  ) {
    def print(): Unit = {
      println("--------------------")
      println(s"#steps: ${classifyCounts.steps}")
      println(s"#pushEnv: $countPushesEnv")
      println(s"maxDepthEnv: $maxDepthEnv")
      println(s"#pushKont: $countPushesKont")
      println(s"maxDepthKont: $maxDepthKont")
      println("--------------------")
      println(s"classify:\n${classifyCounts.pp}")
      println("--------------------")
    }
  }

  private object Instrumentation {
    def apply(): Instrumentation = {
      Instrumentation(
        classifyCounts = new Classify.Counts(),
        countPushesKont = 0,
        countPushesEnv = 0,
        maxDepthKont = 0,
        maxDepthEnv = 0,
      )
    }
  }

  /*
   Speedy uses a caller-saves strategy for managing the environment.  In a Speedy machine,
   the environment is represented by the `frame`, `actuals`, and `env` components.

   We use the terminology "frame" for the array of values which correspond to the
   evaluated "free-vars" of a function closure.

   We use the terminology "actuals" for the array of values which correspond to the
   evaluated "args" of a function application. (The args being an array of expressions)

   The environment "env" is now only used for let-bindings and pattern-matches.

   Continuations are responsible for restoring their own frame/actuals.  On the other
   hand, it is the code which executes a continuation which is responsible for ensuring
   the env-stack of temporaries is popped to the correct height, before the continuation
   is itself executed. See popTempStackToBase.

   When we create/push a continuation which requires it's environment to be preserved, we
   record the current frame and actuals within the continuation. In addition, we call
   markBase to allow the continuation access to temporaries on the env-stack: markBase
   shifts envBase to the current env.size, returning the old envBase, allowing the
   continuation to reset the envBase (calling restoreBase) when it is executed.
   */

  private type Frame = Array[SValue]

  private type Actuals = util.ArrayList[SValue]

  private[lf] sealed trait LedgerMode

  private[lf] final case class CachedContract(
      templateId: Ref.TypeConName,
      value: SValue,
      signatories: Set[Party],
      observers: Set[Party],
      key: Option[Node.KeyWithMaintainers],
  ) {
    private[lf] val stakeholders: Set[Party] = signatories union observers
    private[speedy] val any = SValue.SAny(TTyCon(templateId), value)
  }

  private[this] def enforceLimit(actual: Int, limit: Int, error: Int => IError.Limit.Error) =
    if (actual > limit)
      throw SError.SErrorDamlException(IError.Limit(error(limit)))

  private[lf] final case class OnLedger(
      val validating: Boolean,
      val contractKeyUniqueness: ContractKeyUniquenessMode,
      /* The current partial transaction */
      private[speedy] var ptx: PartialTransaction,
      /* Committers of the action. */
      val committers: Set[Party],
      /* Additional readers (besides committers) for visibility checks. */
      val readAs: Set[Party],
      /* Commit location, if a scenario commit is in progress. */
      val commitLocation: Option[Location],
      /* Flag to trace usage of get_time builtins */
      var dependsOnTime: Boolean,
      // global contract discriminators, that are discriminators from contract created in previous transactions
      var cachedContracts: Map[V.ContractId, CachedContract],
      var numInputContracts: Int,
      val limits: interpretation.Limits,
  ) extends LedgerMode {
    private[lf] val visibleToStakeholders: Set[Party] => SVisibleToStakeholders =
      if (validating) { _ => SVisibleToStakeholders.Visible }
      else {
        SVisibleToStakeholders.fromSubmitters(committers, readAs)
      }
    private[lf] def finish: PartialTransaction.Result = ptx.finish
    private[lf] def ptxInternal: PartialTransaction = ptx // deprecated
    private[lf] def incompleteTransaction: IncompleteTransaction = ptx.finishIncomplete

    private[speedy] def updateCachedContracts(cid: V.ContractId, contract: CachedContract): Unit = {
      enforceLimit(
        contract.signatories.size,
        limits.contractSignatories,
        IError.Limit
          .ContractSignatories(
            cid,
            contract.templateId,
            contract.value.toUnnormalizedValue,
            contract.signatories,
            _,
          ),
      )
      enforceLimit(
        contract.observers.size,
        limits.contractObservers,
        IError.Limit
          .ContractObservers(
            cid,
            contract.templateId,
            contract.value.toUnnormalizedValue,
            contract.observers,
            _,
          ),
      )
      cachedContracts = cachedContracts.updated(cid, contract)
    }

    private[speedy] def addGlobalContract(coid: V.ContractId, contract: CachedContract): Unit = {
      numInputContracts += 1
      enforceLimit(
        numInputContracts,
        limits.transactionInputContracts,
        IError.Limit.TransactionInputContracts,
      )
      updateCachedContracts(coid, contract)
    }

    private[speedy] def enforceChoiceControllersLimit(
        controllers: Set[Party],
        cid: V.ContractId,
        templateId: TypeConName,
        choiceName: ChoiceName,
        arg: V,
    ): Unit =
      enforceLimit(
        controllers.size,
        limits.choiceControllers,
        IError.Limit.ChoiceControllers(cid, templateId, choiceName, arg, controllers, _),
      )

    private[speedy] def enforceChoiceObserversLimit(
        observers: Set[Party],
        cid: V.ContractId,
        templateId: TypeConName,
        choiceName: ChoiceName,
        arg: V,
    ): Unit =
      enforceLimit(
        observers.size,
        limits.choiceObservers,
        IError.Limit.ChoiceObservers(cid, templateId, choiceName, arg, observers, _),
      )

  }

  private[lf] final case object OffLedger extends LedgerMode

  /** The speedy CEK machine. */
  final class Machine(
      /* The control is what the machine should be evaluating. If this is not
       * null, then `returnValue` must be null.
       */
      var ctrl: SExpr,
      /* `returnValue` contains the result once the expression in `ctrl` has
       * been fully evaluated. If this is not null, then `ctrl` must be null.
       */
      var returnValue: SValue,
      /* Frame: to access values for a closure's free-vars. */
      var frame: Frame,
      /* Actuals: to access values for a function application's arguments. */
      var actuals: Actuals,
      /* [env] is a stack of temporary values for: let-bindings and pattern-matches. */
      var env: Env,
      /* [envBase] is the depth of the temporaries-stack when the current code-context was
       * begun. We revert to this depth when entering a closure, or returning to the top
       * continuation on the kontStack.
       */
      var envBase: Int,
      /* Kont, or continuation specifies what should be done next
       * once the control has been evaluated.
       */
      var kontStack: util.ArrayList[Kont],
      /* The last encountered location */
      var lastLocation: Option[Location],
      /* The trace log. */
      val traceLog: TraceLog,
      /* Engine-generated warnings. */
      val warningLog: WarningLog,
      /* loggingContext */
      implicit val loggingContext: LoggingContext,
      /* Compiled packages (Daml-LF ast + compiled speedy expressions). */
      var compiledPackages: CompiledPackages,
      /* Used when enableLightweightStepTracing is true */
      var steps: Int,
      /* Used when enableInstrumentation is true */
      var track: Instrumentation,
      /* Profile of the run when the packages haven been compiled with profiling enabled. */
      var profile: Profile,
      /* True if we are running on ledger building transactions, false if we
         are running off-ledger code, e.g., Daml Script or
         Triggers. It is safe to use on ledger for off ledger code but
         not the other way around.
       */
      val ledgerMode: LedgerMode,
  ) {

    def tmplId2TxVersion(tmplId: TypeConName) =
      TransactionVersion.assignNodeVersion(
        compiledPackages.interface.packageLanguageVersion(tmplId.packageId)
      )

    def normValue(templateId: TypeConName, svalue: SValue): V =
      svalue.toNormalizedValue(tmplId2TxVersion(templateId))

    /* kont manipulation... */

    @inline
    private[speedy] def kontDepth(): Int = kontStack.size()

    private[lf] def withOnLedger[T](location: String)(f: OnLedger => T): T =
      ledgerMode match {
        case onLedger: OnLedger => f(onLedger)
        case OffLedger => throw SErrorCrash(location, "unexpected off-ledger machine")
      }

    @inline
    private[speedy] def pushKont(k: Kont): Unit = {
      discard[Boolean](kontStack.add(k))
      if (enableInstrumentation) {
        track.countPushesKont += 1
        if (kontDepth() > track.maxDepthKont) track.maxDepthKont = kontDepth()
      }
    }

    @inline
    private[speedy] def popKont(): Kont = {
      kontStack.remove(kontStack.size - 1)
    }

    /* env manipulation... */

    // The environment is partitioned into three locations: Stack, Args, Free
    // The run-time location of a variable is determined (at compile time) by closureConvert
    // And made explicit by a specifc speedy expression node: SELocS/SELocA/SELocF
    // At runtime these different location-node execute by calling the corresponding `getEnv*` function

    // Variables which reside on the stack. Indexed (from 1) by relative offset from the top of the stack (1 is top!)
    @inline
    private[speedy] def getEnvStack(i: Int): SValue = env.get(env.size - i)

    // Variables which reside in the args array of the current frame. Indexed by absolute offset.
    @inline
    private[speedy] def getEnvArg(i: Int): SValue = actuals.get(i)

    // Variables which reside in the free-vars array of the current frame. Indexed by absolute offset.
    @inline
    private[speedy] def getEnvFree(i: Int): SValue = frame(i)

    @inline def pushEnv(v: SValue): Unit = {
      discard[Boolean](env.add(v))
      if (enableInstrumentation) {
        track.countPushesEnv += 1
        if (env.size > track.maxDepthEnv) track.maxDepthEnv = env.size
      }
    }

    // markBase is called when pushing a continuation which requires access to temporaries
    // currently on the env-stack.  After this call, envBase is set to the current
    // env.size. The old envBase is returned so it can be restored later by the caller.
    @inline
    def markBase(): Int = {
      val oldBase = this.envBase
      val newBase = this.env.size
      if (newBase < oldBase) {
        throw SErrorCrash(
          NameOf.qualifiedNameOfCurrentFunc,
          s"markBase: $oldBase -> $newBase -- NOT AN INCREASE",
        )
      }
      this.envBase = newBase
      oldBase
    }

    // restoreBase is called when executing a continuation which previously saved the
    // value of envBase (by calling markBase).
    @inline
    def restoreBase(envBase: Int): Unit = {
      if (this.envBase < envBase) {
        throw SErrorCrash(
          NameOf.qualifiedNameOfCurrentFunc,
          s"restoreBase: ${this.envBase} -> ${envBase} -- NOT A REDUCTION",
        )
      }
      this.envBase = envBase
    }

    // popTempStackToBase is called when we begin a new code-context which does not need
    // to access any temporaries pushed to the stack by the current code-context. This
    // occurs either when returning to the top continuation on the kontStack or when
    // entering (tail-calling) a closure.
    @inline
    def popTempStackToBase(): Unit = {
      val envSizeToBeRestored = this.envBase
      val count = env.size - envSizeToBeRestored
      if (count < 0) {
        throw SErrorCrash(
          NameOf.qualifiedNameOfCurrentFunc,
          s"popTempStackToBase: ${env.size} --> ${envSizeToBeRestored} -- WRONG DIRECTION",
        )
      }
      if (count > 0) {
        env.subList(envSizeToBeRestored, env.size).clear
      }
    }

    @inline
    def restoreFrameAndActuals(frame: Frame, actuals: Actuals): Unit = {
      // Restore the frame and actuals to the state when the continuation was created.
      this.frame = frame
      this.actuals = actuals
    }

    /** Push a single location to the continuation stack for the sake of
      *        maintaining a stack trace.
      */
    def pushLocation(loc: Location): Unit = {
      lastLocation = Some(loc)
      val last_index = kontStack.size() - 1
      val last_kont = if (last_index >= 0) Some(kontStack.get(last_index)) else None
      last_kont match {
        // NOTE(MH): If the top of the continuation stack is the monadic token,
        // we push location information under it to account for the implicit
        // lambda binding the token.
        case Some(KArg(_, Array(SEValue.Token))) => {
          // Can't call pushKont here, because we don't push at the top of the stack.
          kontStack.add(last_index, KLocation(this, loc))
          if (enableInstrumentation) {
            track.countPushesKont += 1
            if (kontDepth() > track.maxDepthKont) track.maxDepthKont = kontDepth()
          }
        }
        // NOTE(MH): When we use a cached top level value, we need to put the
        // stack trace it produced back on the continuation stack to get
        // complete stack trace at the use site. Thus, we store the stack traces
        // of top level values separately during their execution.
        case Some(KCacheVal(machine, v, defn, stack_trace)) =>
          discard(kontStack.set(last_index, KCacheVal(machine, v, defn, loc :: stack_trace)))
        case _ => pushKont(KLocation(this, loc))
      }
    }

    /** Push an entire stack trace to the continuation stack. The first
      *        element of the list will be pushed last.
      */
    def pushStackTrace(locs: List[Location]): Unit =
      locs.reverse.foreach(pushLocation)

    /** Compute a stack trace from the locations in the continuation stack.
      *        The last seen location will come last.
      */
    def stackTrace(): ImmArray[Location] = {
      val s = ImmArray.newBuilder[Location]
      kontStack.forEach { k =>
        k match {
          case KLocation(_, location) => discard(s += location)
          case _ => ()
        }
      }
      s.result()
    }

    private[lf] def contextActors: Set[Party] =
      withOnLedger("ptx") { onLedger =>
        onLedger.ptx.context.info.authorizers
      }

    private[lf] def auth: Authorize = Authorize(this.contextActors)

    /** Reuse an existing speedy machine to evaluate a new expression.
      *      Do not use if the machine is partway though an existing evaluation.
      *      i.e. run() has returned an `SResult` requiring a callback.
      */
    def setExpressionToEvaluate(expr: SExpr): Unit = {
      ctrl = expr
      kontStack = initialKontStack()
      env = emptyEnv
      envBase = 0
      steps = 0
      track = Instrumentation()
    }

    /** Run a machine until we get a result: either a final-value or a request for data, with a callback */
    def run(): SResult = {
      try {
        // normal exit from this loop is when KFinished.execute throws SpeedyHungry
        @tailrec
        def loop(): SResult = {
          if (enableInstrumentation) {
            Classify.classifyMachine(this, track.classifyCounts)
          }
          if (enableLightweightStepTracing) {
            steps += 1
            println(s"$steps: ${PrettyLightweight.ppMachine(this)}")
          }
          if (returnValue != null) {
            val value = returnValue
            returnValue = null
            popTempStackToBase()
            popKont().execute(value)
          } else {
            val expr = ctrl
            ctrl = null
            expr.execute(this)
          }
          loop()
        }
        loop()
      } catch {
        case SpeedyHungry(res: SResult) =>
          if (enableInstrumentation) {
            res match {
              case _: SResultFinalValue => track.print()
              case _ => ()
            }
          }
          res
        case serr: SError =>
          SResultError(serr)
        case ex: RuntimeException =>
          SResultError(SErrorCrash(NameOf.qualifiedNameOfCurrentFunc, s"exception: $ex")) // stop
      }
    }

    def lookupVal(eval: SEVal): Unit = {
      eval.cached match {
        case Some((v, stack_trace)) =>
          pushStackTrace(stack_trace)
          returnValue = v

        case None =>
          val ref = eval.ref
          compiledPackages.getDefinition(ref) match {
            case Some(defn) =>
              defn.cached match {
                case Some((svalue, stackTrace)) =>
                  eval.setCached(svalue, stackTrace)
                  returnValue = svalue
                case None =>
                  pushKont(KCacheVal(this, eval, defn, Nil))
                  ctrl = defn.body
              }
            case None =>
              if (compiledPackages.packageIds.contains(ref.packageId))
                throw SErrorCrash(
                  NameOf.qualifiedNameOfCurrentFunc,
                  s"definition $ref not found even after caller provided new set of packages",
                )
              else
                throw SpeedyHungry(
                  SResultNeedPackage(
                    ref.packageId,
                    language.Reference.Package(ref.packageId),
                    { packages =>
                      this.compiledPackages = packages
                      // To avoid infinite loop in case the packages are not updated properly by the caller
                      assert(compiledPackages.packageIds.contains(ref.packageId))
                      ctrl = eval
                    },
                  )
                )
          }
      }
    }

    /** This function is used to enter an ANF application.  The function has been evaluated to
      *      a value, and so have the arguments - they just need looking up
      */
    // TODO: share common code with executeApplication
    private[speedy] def enterApplication(vfun: SValue, newArgs: Array[SExprAtomic]): Unit = {
      vfun match {
        case SValue.SPAP(prim, actualsSoFar, arity) =>
          val missing = arity - actualsSoFar.size
          val newArgsLimit = Math.min(missing, newArgs.length)

          val actuals = new util.ArrayList[SValue](actualsSoFar.size + newArgsLimit)
          discard[Boolean](actuals.addAll(actualsSoFar))

          val othersLength = newArgs.length - missing

          // Evaluate the arguments
          var i = 0
          while (i < newArgsLimit) {
            val newArg = newArgs(i)
            val v = newArg.lookupValue(this)
            discard[Boolean](actuals.add(v))
            i += 1
          }

          // Not enough arguments. Return a PAP.
          if (othersLength < 0) {
            this.returnValue = SValue.SPAP(prim, actuals, arity)

          } else {
            // Too many arguments: Push a continuation to re-apply the over-applied args.
            if (othersLength > 0) {
              val others = new Array[SExprAtomic](othersLength)
              System.arraycopy(newArgs, missing, others, 0, othersLength)
              this.pushKont(KOverApp(this, others))
            }
            // Now the correct number of arguments is ensured. What kind of prim do we have?
            prim match {
              case closure: SValue.PClosure =>
                this.frame = closure.frame
                this.actuals = actuals
                // Maybe push a continuation for the profiler
                val label = closure.label
                if (label != null) {
                  this.profile.addOpenEvent(label)
                  this.pushKont(KLeaveClosure(this, label))
                }
                // Start evaluating the body of the closure.
                popTempStackToBase()
                this.ctrl = closure.expr

              case SValue.PBuiltin(builtin) =>
                this.actuals = actuals
                builtin.execute(actuals, this)
            }
          }

        case _ =>
          throw SErrorCrash(NameOf.qualifiedNameOfCurrentFunc, s"Applying non-PAP: $vfun")
      }
    }

    /** The function has been evaluated to a value, now start evaluating the arguments. */
    private[speedy] def executeApplication(vfun: SValue, newArgs: Array[SExpr]): Unit = {
      vfun match {
        case SValue.SPAP(prim, actualsSoFar, arity) =>
          val missing = arity - actualsSoFar.size
          val newArgsLimit = Math.min(missing, newArgs.length)

          val actuals = new util.ArrayList[SValue](actualsSoFar.size + newArgsLimit)
          discard[Boolean](actuals.addAll(actualsSoFar))

          val othersLength = newArgs.length - missing

          // Not enough arguments. Push a continuation to construct the PAP.
          if (othersLength < 0) {
            this.pushKont(KPap(this, prim, actuals, arity))
          } else {
            // Too many arguments: Push a continuation to re-apply the over-applied args.
            if (othersLength > 0) {
              val others = new Array[SExpr](othersLength)
              System.arraycopy(newArgs, missing, others, 0, othersLength)
              this.pushKont(KArg(this, others))
            }
            // Now the correct number of arguments is ensured. What kind of prim do we have?
            prim match {
              case closure: SValue.PClosure =>
                // Push a continuation to execute the function body when the arguments have been evaluated
                this.pushKont(KFun(this, closure, actuals))

              case SValue.PBuiltin(builtin) =>
                // Push a continuation to execute the builtin when the arguments have been evaluated
                this.pushKont(KBuiltin(this, builtin, actuals))
            }
          }
          this.evaluateArguments(actuals, newArgs, newArgsLimit)

        case _ =>
          throw SErrorCrash(NameOf.qualifiedNameOfCurrentFunc, s"Applying non-PAP: $vfun")
      }
    }

    /** Evaluate the first 'n' arguments in 'args'.
      *      'args' will contain at least 'n' expressions, but it may contain more(!)
      *
      *      This is because, in the call from 'executeApplication' below, although over-applied
      *      arguments are pushed into a continuation, they are not removed from the original array
      *      which is passed here as 'args'.
      */
    private[speedy] def evaluateArguments(
        actuals: util.ArrayList[SValue],
        args: Array[SExpr],
        n: Int,
    ) = {
      var i = 1
      while (i < n) {
        val arg = args(n - i)
        this.pushKont(KPushTo(this, actuals, arg))
        i = i + 1
      }
      this.ctrl = args(0)
    }

    private[speedy] def print(count: Int) = {
      println(s"Step: $count")
      if (returnValue != null) {
        println("Control: null")
        println("Return:")
        println(s"  ${returnValue}")
      } else {
        println("Control:")
        println(s"  ${ctrl}")
        println("Return: null")
      }
      println("Environment:")
      env.forEach { v =>
        println("  " + v.toString)
      }
      println("Kontinuation:")
      kontStack.forEach { k =>
        println(s"  " + k.toString)
      }
      println("============================================================")
    }

    // This translates a well-typed LF value (typically coming from the ledger)
    // to speedy value and set the control of with the result.
    // Note the method does not check the value is well-typed as opposed as
    // com.daml.lf.engine.preprocessing.ValueTranslator.translateValue.
    // All the contract IDs contained in the value are considered global.
    // Raises an exception if missing a package.
    private[speedy] def importValue(typ0: Type, value0: V): Unit = {

      def assertRight[X](x: Either[LookupError, X]) =
        x match {
          case Right(value) => value
          case Left(error) => throw SErrorCrash(NameOf.qualifiedNameOfCurrentFunc, error.pretty)
        }

      def go(ty: Type, value: V): SValue = {
        def typeMismatch = throw SErrorCrash(
          NameOf.qualifiedNameOfCurrentFunc,
          s"mismatching type: $ty and value: $value",
        )

        val (tyFun, argTypes) = AstUtil.destructApp(ty)
        tyFun match {
          case TBuiltin(_) =>
            argTypes match {
              case Nil =>
                value match {
                  case V.ValueInt64(value) =>
                    SValue.SInt64(value)
                  case V.ValueNumeric(value) =>
                    SValue.SNumeric(value)
                  case V.ValueText(value) =>
                    SValue.SText(value)
                  case V.ValueTimestamp(value) =>
                    SValue.STimestamp(value)
                  case V.ValueDate(value) =>
                    SValue.SDate(value)
                  case V.ValueParty(value) =>
                    SValue.SParty(value)
                  case V.ValueBool(b) =>
                    if (b) SValue.SValue.True else SValue.SValue.False
                  case V.ValueUnit =>
                    SValue.SValue.Unit
                  case _ =>
                    typeMismatch
                }
              case elemType :: Nil =>
                value match {
                  case V.ValueContractId(cid) =>
                    SValue.SContractId(cid)
                  case V.ValueNumeric(d) =>
                    SValue.SNumeric(d)
                  case V.ValueOptional(mb) =>
                    mb match {
                      case Some(value) => SValue.SOptional(Some(go(elemType, value)))
                      case None => SValue.SValue.None
                    }
                  // list
                  case V.ValueList(ls) =>
                    SValue.SList(ls.map(go(elemType, _)))

                  // textMap
                  case V.ValueTextMap(entries) =>
                    SValue.SMap(
                      isTextMap = true,
                      entries = entries.iterator.map { case (k, v) =>
                        SValue.SText(k) -> go(elemType, v)
                      },
                    )
                  case _ =>
                    typeMismatch
                }
              case keyType :: valueType :: Nil =>
                value match {
                  // genMap
                  case V.ValueGenMap(entries) =>
                    SValue.SMap(
                      isTextMap = false,
                      entries = entries.iterator.map { case (k, v) =>
                        go(keyType, k) -> go(valueType, v)
                      },
                    )
                  case _ =>
                    typeMismatch
                }
              case _ =>
                typeMismatch
            }
          case TTyCon(tyCon) =>
            value match {
              case V.ValueRecord(_, fields) =>
                val lookupResult =
                  assertRight(compiledPackages.interface.lookupDataRecord(tyCon))
                lazy val subst = lookupResult.subst(argTypes)
                val values = (lookupResult.dataRecord.fields.iterator zip fields.iterator)
                  .map { case ((_, fieldType), (_, fieldValue)) =>
                    go(AstUtil.substitute(fieldType, subst), fieldValue)
                  }
                  .to(ArrayList)
                SValue.SRecord(tyCon, lookupResult.dataRecord.fields.map(_._1), values)
              case V.ValueVariant(_, constructor, value) =>
                val info =
                  assertRight(
                    compiledPackages.interface.lookupVariantConstructor(tyCon, constructor)
                  )
                val valType = info.concreteType(argTypes)
                SValue.SVariant(tyCon, constructor, info.rank, go(valType, value))
              case V.ValueEnum(_, constructor) =>
                val rank =
                  assertRight(compiledPackages.interface.lookupEnumConstructor(tyCon, constructor))
                SValue.SEnum(tyCon, constructor, rank)
              case _ =>
                typeMismatch
            }
          case _ =>
            typeMismatch
        }
      }

      returnValue = go(typ0, value0)
    }

    def checkContractVisibility(onLedger: OnLedger, cid: V.ContractId, contract: CachedContract) = {
      onLedger.visibleToStakeholders(contract.stakeholders) match {
        case SVisibleToStakeholders.Visible => ()
        case SVisibleToStakeholders.NotVisible(actAs, readAs) =>
          val readers = (actAs union readAs).mkString(",")
          val stakeholders = contract.stakeholders.mkString(",")
          this.warningLog.add(
            Warning(
              commitLocation = onLedger.commitLocation,
              message =
                s"Tried to fetch or exercise ${contract.templateId} on contract ${cid.coid} but none of the reading parties [${readers}] are contract stakeholders [${stakeholders}]. Use of divulged contracts is deprecated and incompatible with pruning. To remedy, add one of the readers [${readers}] as an observer to the contract.",
            )
          )
      }
    }

    @throws[SError]
    def checkKeyVisibility(
        onLedger: OnLedger,
        gkey: GlobalKey,
        coid: V.ContractId,
        handleKeyFound: (Machine, V.ContractId) => Unit,
    ): Unit =
      onLedger.cachedContracts.get(coid) match {
        case Some(cachedContract) =>
          val stakeholders = cachedContract.signatories union cachedContract.observers
          onLedger.visibleToStakeholders(stakeholders) match {
            case SVisibleToStakeholders.NotVisible(actAs, readAs) =>
              throw SErrorDamlException(
                interpretation.Error
                  .ContractKeyNotVisible(coid, gkey, actAs, readAs, stakeholders)
              )
            case _ =>
              handleKeyFound(this, coid)
          }
        case None =>
          throw SErrorCrash(
            NameOf.qualifiedNameOfCurrentFunc,
            s"contract ${coid.coid} not in cachedContracts",
          )
      }

  }

  object Machine {

    private[this] val damlTraceLog = ContextualizedLogger.createFor("daml.tracelog")
    private[this] val damlWarnings = ContextualizedLogger.createFor("daml.warnings")

    def newTraceLog: TraceLog = new RingBufferTraceLog(damlTraceLog, 100)
    def newWarningLog: WarningLog = new WarningLog(damlWarnings)

    def apply(
        compiledPackages: CompiledPackages,
        submissionTime: Time.Timestamp,
        initialSeeding: InitialSeeding,
        expr: SExpr,
        committers: Set[Party],
        readAs: Set[Party],
        validating: Boolean = false,
        traceLog: TraceLog = newTraceLog,
        warningLog: WarningLog = newWarningLog,
        contractKeyUniqueness: ContractKeyUniquenessMode = ContractKeyUniquenessMode.Strict,
        commitLocation: Option[Location] = None,
        limits: interpretation.Limits = interpretation.Limits.Lenient,
    )(implicit loggingContext: LoggingContext): Machine = {
      new Machine(
        ctrl = expr,
        returnValue = null,
        frame = null,
        actuals = null,
        env = emptyEnv,
        envBase = 0,
        kontStack = initialKontStack(),
        lastLocation = None,
        ledgerMode = OnLedger(
          validating = validating,
          ptx = PartialTransaction
            .initial(
              contractKeyUniqueness,
              submissionTime,
              initialSeeding,
              committers,
            ),
          committers = committers,
          readAs = readAs,
          commitLocation = commitLocation,
          dependsOnTime = false,
          cachedContracts = Map.empty,
          numInputContracts = 0,
          contractKeyUniqueness = contractKeyUniqueness,
          limits = limits,
        ),
        traceLog = traceLog,
        warningLog = warningLog,
        loggingContext = loggingContext,
        compiledPackages = compiledPackages,
        steps = 0,
        track = Instrumentation(),
        profile = new Profile(),
      )
    }

    @throws[PackageNotFound]
    @throws[CompilationError]
    // Construct a machine for running an update expression (testing -- avoiding scenarios)
    def fromUpdateExpr(
        compiledPackages: CompiledPackages,
        transactionSeed: crypto.Hash,
        updateE: Expr,
        committers: Set[Party],
        limits: interpretation.Limits = interpretation.Limits.Lenient,
    )(implicit loggingContext: LoggingContext): Machine = {
      val updateSE: SExpr = compiledPackages.compiler.unsafeCompile(updateE)
      fromUpdateSExpr(compiledPackages, transactionSeed, updateSE, committers, limits)
    }

    @throws[PackageNotFound]
    @throws[CompilationError]
    // Construct a machine for running an update expression (testing -- avoiding scenarios)
    def fromUpdateSExpr(
        compiledPackages: CompiledPackages,
        transactionSeed: crypto.Hash,
        updateSE: SExpr,
        committers: Set[Party],
        limits: interpretation.Limits = interpretation.Limits.Lenient,
        traceLog: TraceLog = newTraceLog,
    )(implicit loggingContext: LoggingContext): Machine = {
      Machine(
        compiledPackages = compiledPackages,
        submissionTime = Time.Timestamp.MinValue,
        initialSeeding = InitialSeeding.TransactionSeed(transactionSeed),
        expr = SEApp(updateSE, Array(SEValue.Token)),
        committers = committers,
        readAs = Set.empty,
        limits = limits,
        traceLog = traceLog,
      )
    }

    @throws[PackageNotFound]
    @throws[CompilationError]
    // Construct a machine for running scenario.
    def fromScenarioSExpr(
        compiledPackages: CompiledPackages,
        scenario: SExpr,
    )(implicit loggingContext: LoggingContext): Machine = Machine.fromPureSExpr(
      compiledPackages = compiledPackages,
      expr = SEApp(scenario, Array(SEValue.Token)),
    )

    @throws[PackageNotFound]
    @throws[CompilationError]
    // Construct a machine for running scenario.
    def fromScenarioExpr(
        compiledPackages: CompiledPackages,
        scenario: Expr,
    )(implicit loggingContext: LoggingContext): Machine =
      fromScenarioSExpr(
        compiledPackages = compiledPackages,
        scenario = compiledPackages.compiler.unsafeCompile(scenario),
      )

    @throws[PackageNotFound]
    @throws[CompilationError]
    // Construct a machine for evaluating an expression that is neither an update nor a scenario expression.
    def fromPureSExpr(
        compiledPackages: CompiledPackages,
        expr: SExpr,
        traceLog: TraceLog = newTraceLog,
        warningLog: WarningLog = newWarningLog,
    )(implicit loggingContext: LoggingContext): Machine =
      new Machine(
        ctrl = expr,
        returnValue = null,
        frame = null,
        actuals = null,
        env = emptyEnv,
        envBase = 0,
        kontStack = initialKontStack(),
        lastLocation = None,
        ledgerMode = OffLedger,
        traceLog = traceLog,
        warningLog = warningLog,
        loggingContext = loggingContext,
        compiledPackages = compiledPackages,
        steps = 0,
        track = Instrumentation(),
        profile = new Profile(),
      )

    @throws[PackageNotFound]
    @throws[CompilationError]
    // Construct a machine for evaluating an expression that is neither an update nor a scenario expression.
    def fromPureExpr(
        compiledPackages: CompiledPackages,
        expr: Expr,
    )(implicit loggingContext: LoggingContext): Machine =
      fromPureSExpr(compiledPackages, compiledPackages.compiler.unsafeCompile(expr))

  }

  // Environment
  //
  // NOTE(JM): We use ArrayList instead of ArrayBuffer as
  // it is significantly faster.
  private[speedy] type Env = util.ArrayList[SValue]
  private[speedy] def emptyEnv: Env = new util.ArrayList[SValue](512)

  //
  // Kontinuation
  //
  // Whilst the machine is running, we ensure the kontStack is *never* empty.
  // We do this by pushing a KFinished continutaion on the initially empty stack, which
  // returns the final result (by raising it as a SpeedyHungry exception).

  private[this] def initialKontStack(): util.ArrayList[Kont] = {
    val kontStack = new util.ArrayList[Kont](128)
    discard[Boolean](kontStack.add(KFinished))
    kontStack
  }

  /** Kont, or continuation. Describes the next step for the machine
    * after an expression has been evaluated into a 'SValue'.
    */
  private[speedy] sealed trait Kont {

    /** Execute the continuation. */
    def execute(v: SValue): Unit
  }

  /** Final continuation; machine has computed final value */
  private[speedy] final case object KFinished extends Kont {
    def execute(v: SValue) = {
      throw SpeedyHungry(SResultFinalValue(v))
    }
  }

  private[speedy] final case class KOverApp(machine: Machine, newArgs: Array[SExprAtomic])
      extends Kont
      with SomeArrayEquals {

    private[this] val savedBase = machine.markBase()
    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    def execute(vfun: SValue) = {
      machine.restoreBase(savedBase);
      machine.restoreFrameAndActuals(frame, actuals)
      machine.enterApplication(vfun, newArgs)
    }
  }

  /** The function has been evaluated to a value. Now restore the environment and execute the application */
  private[speedy] final case class KArg(
      machine: Machine,
      newArgs: Array[SExpr],
  ) extends Kont
      with SomeArrayEquals {

    private[this] val savedBase = machine.markBase()
    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    def execute(vfun: SValue) = {
      machine.restoreBase(savedBase);
      machine.restoreFrameAndActuals(frame, actuals)
      machine.executeApplication(vfun, newArgs)
    }
  }

  /** The function-closure and arguments have been evaluated. Now execute the body. */
  private[speedy] final case class KFun(
      machine: Machine,
      closure: SValue.PClosure,
      actuals: util.ArrayList[SValue],
  ) extends Kont
      with SomeArrayEquals {

    private[this] val savedBase = machine.markBase()

    def execute(v: SValue) = {
      discard[Boolean](actuals.add(v))
      // Set frame/actuals to allow access to the function arguments and closure free-varables.
      machine.restoreBase(savedBase)
      machine.restoreFrameAndActuals(closure.frame, actuals)
      // Maybe push a continuation for the profiler
      val label = closure.label
      if (label != null) {
        machine.profile.addOpenEvent(label)
        machine.pushKont(KLeaveClosure(machine, label))
      }
      // Start evaluating the body of the closure.
      machine.popTempStackToBase()
      machine.ctrl = closure.expr
    }
  }

  /** The builtin arguments have been evaluated. Now execute the builtin. */
  private[speedy] final case class KBuiltin(
      machine: Machine,
      builtin: SBuiltin,
      actuals: util.ArrayList[SValue],
  ) extends Kont {

    private[this] val savedBase = machine.markBase()

    def execute(v: SValue) = {
      discard[Boolean](actuals.add(v))
      // A builtin has no free-vars, so we set the frame to null.
      machine.restoreBase(savedBase)
      machine.restoreFrameAndActuals(null, actuals)
      builtin.execute(actuals, machine)
    }
  }

  /** The function's partial-arguments have been evaluated. Construct and return the PAP */
  private[speedy] final case class KPap(
      machine: Machine,
      prim: SValue.Prim,
      actuals: util.ArrayList[SValue],
      arity: Int,
  ) extends Kont {

    def execute(v: SValue) = {
      discard[Boolean](actuals.add(v))
      machine.returnValue = SValue.SPAP(prim, actuals, arity)
    }
  }

  /** The scrutinee of a match has been evaluated, now match the alternatives against it. */
  private[speedy] def executeMatchAlts(machine: Machine, alts: Array[SCaseAlt], v: SValue): Unit = {
    val altOpt = v match {
      case SValue.SBool(b) =>
        alts.find { alt =>
          alt.pattern match {
            case SCPPrimCon(PCTrue) => b
            case SCPPrimCon(PCFalse) => !b
            case SCPDefault => true
            case _ => false
          }
        }
      case SValue.SVariant(_, _, rank1, arg) =>
        alts.find { alt =>
          alt.pattern match {
            case SCPVariant(_, _, rank2) if rank1 == rank2 =>
              machine.pushEnv(arg)
              true
            case SCPDefault => true
            case _ => false
          }
        }
      case SValue.SEnum(_, _, rank1) =>
        alts.find { alt =>
          alt.pattern match {
            case SCPEnum(_, _, rank2) => rank1 == rank2
            case SCPDefault => true
            case _ => false
          }
        }
      case SValue.SList(lst) =>
        alts.find { alt =>
          alt.pattern match {
            case SCPNil if lst.isEmpty => true
            case SCPCons if !lst.isEmpty =>
              val Some((head, tail)) = lst.pop
              machine.pushEnv(head)
              machine.pushEnv(SValue.SList(tail))
              true
            case SCPDefault => true
            case _ => false
          }
        }
      case SValue.SUnit =>
        alts.find { alt =>
          alt.pattern match {
            case SCPPrimCon(PCUnit) => true
            case SCPDefault => true
            case _ => false
          }
        }
      case SValue.SOptional(mbVal) =>
        alts.find { alt =>
          alt.pattern match {
            case SCPNone if mbVal.isEmpty => true
            case SCPSome =>
              mbVal match {
                case None => false
                case Some(x) =>
                  machine.pushEnv(x)
                  true
              }
            case SCPDefault => true
            case _ => false
          }
        }
      case SValue.SContractId(_) | SValue.SDate(_) | SValue.SNumeric(_) | SValue.SInt64(_) |
          SValue.SParty(_) | SValue.SText(_) | SValue.STimestamp(_) | SValue.SStruct(_, _) |
          SValue.SMap(_, _) | SValue.SRecord(_, _, _) | SValue.SAny(_, _) | SValue.STypeRep(_) |
          SValue.STNat(_) | SValue.SBigNumeric(_) | _: SValue.SPAP | SValue.SToken =>
        throw SErrorCrash(NameOf.qualifiedNameOfCurrentFunc, "Match on non-matchable value")
    }

    machine.ctrl = altOpt
      .getOrElse(
        throw SErrorCrash(NameOf.qualifiedNameOfCurrentFunc, s"No match for $v in ${alts.toList}")
      )
      .body
  }

  private[speedy] final case class KMatch(machine: Machine, alts: Array[SCaseAlt])
      extends Kont
      with SomeArrayEquals {

    private[this] val savedBase = machine.markBase()
    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    def execute(v: SValue) = {
      machine.restoreBase(savedBase);
      machine.restoreFrameAndActuals(frame, actuals)
      executeMatchAlts(machine, alts, v)
    }
  }

  /** Push the evaluated value to the array 'to', and start evaluating the expression 'next'.
    * This continuation is used to implement both function application and lets. In
    * the case of function application the arguments are pushed into the 'actuals' array of
    * the PAP that is being built, and in the case of lets the evaluated value is pushed
    * direy into the environment.
    */
  private[speedy] final case class KPushTo(
      machine: Machine,
      to: util.ArrayList[SValue],
      next: SExpr,
  ) extends Kont
      with SomeArrayEquals {

    private[this] val savedBase = machine.markBase()
    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    def execute(v: SValue) = {
      machine.restoreBase(savedBase);
      machine.restoreFrameAndActuals(frame, actuals)
      discard[Boolean](to.add(v))
      machine.ctrl = next
    }
  }

  private[speedy] final case class KFoldl(
      machine: Machine,
      func: SValue,
      var list: FrontStack[SValue],
  ) extends Kont
      with SomeArrayEquals {

    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    def execute(acc: SValue) = {
      list.pop match {
        case None =>
          machine.returnValue = acc
        case Some((item, rest)) =>
          machine.restoreFrameAndActuals(frame, actuals)
          // NOTE: We are "recycling" the current continuation with the
          // remainder of the list to avoid allocating a new continuation.
          list = rest
          machine.pushKont(this)
          machine.enterApplication(func, Array(SEValue(acc), SEValue(item)))
      }
    }
  }

  private[speedy] final case class KFoldr(
      machine: Machine,
      func: SValue,
      list: ImmArray[SValue],
      var lastIndex: Int,
  ) extends Kont
      with SomeArrayEquals {

    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    def execute(acc: SValue) = {
      if (lastIndex > 0) {
        machine.restoreFrameAndActuals(frame, actuals)
        val currentIndex = lastIndex - 1
        val item = list(currentIndex)
        lastIndex = currentIndex
        machine.pushKont(this) // NOTE: We've updated `lastIndex`.
        machine.enterApplication(func, Array(SEValue(item), SEValue(acc)))
      } else {
        machine.returnValue = acc
      }
    }
  }

  // NOTE: See the explanation above the definition of `SBFoldr` on why we need
  // this continuation and what it does.
  private[speedy] final case class KFoldr1Map(
      machine: Machine,
      func: SValue,
      var list: FrontStack[SValue],
      var revClosures: FrontStack[SValue],
      init: SValue,
  ) extends Kont
      with SomeArrayEquals {

    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    def execute(closure: SValue) = {
      revClosures = closure +: revClosures
      list.pop match {
        case None =>
          machine.pushKont(KFoldr1Reduce(machine, revClosures))
          machine.returnValue = init
        case Some((item, rest)) =>
          machine.restoreFrameAndActuals(frame, actuals)
          list = rest
          machine.pushKont(this) // NOTE: We've updated `revClosures` and `list`.
          machine.enterApplication(func, Array(SEValue(item)))
      }
    }
  }

  // NOTE: See the explanation above the definition of `SBFoldr` on why we need
  // this continuation and what it does.
  private[speedy] final case class KFoldr1Reduce(
      machine: Machine,
      var revClosures: FrontStack[SValue],
  ) extends Kont
      with SomeArrayEquals {

    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    def execute(acc: SValue) = {
      revClosures.pop match {
        case None =>
          machine.returnValue = acc
        case Some((closure, rest)) =>
          machine.restoreFrameAndActuals(frame, actuals)
          revClosures = rest
          machine.pushKont(this) // NOTE: We've updated `revClosures`.
          machine.enterApplication(closure, Array(SEValue(acc)))
      }
    }
  }

  /** Store the evaluated value in the definition and in the 'SEVal' from which the
    * expression came from. This in principle makes top-level values lazy. It is a
    * useful optimization to allow creation of large constants (for example records
    * that are repeatedly accessed. In older compilers which did not use the builtin
    * record and struct updates this solves the blow-up which would happen when a
    * large record is updated multiple times.
    */
  private[speedy] final case class KCacheVal(
      machine: Machine,
      v: SEVal,
      defn: SDefinition,
      stack_trace: List[Location],
  ) extends Kont {

    def execute(sv: SValue): Unit = {
      machine.pushStackTrace(stack_trace)
      v.setCached(sv, stack_trace)
      defn.setCached(sv, stack_trace)
      machine.returnValue = sv
    }
  }

  private[speedy] final case class KCacheContract(machine: Machine, cid: V.ContractId)
      extends Kont {

    def execute(sv: SValue): Unit = {
      machine.withOnLedger("KCacheContract") { onLedger =>
        val cached = SBuiltin.extractCachedContract(machine, sv)
        machine.checkContractVisibility(onLedger, cid, cached)
        onLedger.addGlobalContract(cid, cached)
        machine.returnValue = cached.any
      }
    }

  }

  private[speedy] final case class KCheckKeyVisibitiy(
      machine: Machine,
      gKey: GlobalKey,
      cid: V.ContractId,
      handleKeyFound: (Machine, V.ContractId) => Unit,
  ) extends Kont {
    def execute(sv: SValue): Unit =
      machine.withOnLedger("KCheckKeyVisibitiy")(
        machine.checkKeyVisibility(_, gKey, cid, handleKeyFound)
      )
  }

  /** KCloseExercise. Marks an open-exercise which needs to be closed. Either:
    * (1) by 'endExercises' if this continuation is entered normally, or
    * (2) by 'abortExercises' if we unwind the stack through this continuation
    */
  private[speedy] final case class KCloseExercise(machine: Machine) extends Kont {

    def execute(exerciseResult: SValue) = {
      machine.withOnLedger("KCloseExercise") { onLedger =>
        onLedger.ptx = onLedger.ptx.endExercises(exerciseResult.toNormalizedValue)
        checkAborted(onLedger.ptx)
      }
      machine.returnValue = exerciseResult
    }
  }

  /** KTryCatchHandler marks the kont-stack to allow unwinding when throw is executed. If
    * the continuation is entered normally, the environment is restored but the handler is
    * not executed.  When a throw is executed, the kont-stack is unwound to the nearest
    * enclosing KTryCatchHandler (if there is one), and the code for the handler executed.
    */
  private[speedy] final case class KTryCatchHandler(
      machine: Machine,
      handler: SExpr,
  ) extends Kont
      with SomeArrayEquals {

    private[this] val savedBase = machine.markBase()
    private[this] val frame = machine.frame
    private[this] val actuals = machine.actuals

    // we must restore when catching a throw, or for normal execution
    def restore() = {
      machine.restoreBase(savedBase)
      machine.restoreFrameAndActuals(frame, actuals)
    }

    def execute(v: SValue) = {
      restore()
      machine.withOnLedger("KTryCatchHandler") { onLedger =>
        onLedger.ptx = onLedger.ptx.endTry
      }
      machine.returnValue = v
    }
  }

  private[speedy] final case class KCheckChoiceGuard(
      machine: Machine,
      coid: V.ContractId,
      templateId: TypeConName,
      choiceName: ChoiceName,
      byInterface: Option[TypeConName],
  ) extends Kont {
    def abort[E](): E =
      throw SErrorDamlException(IError.ChoiceGuardFailed(coid, templateId, choiceName, byInterface))

    def execute(v: SValue) = {
      v match {
        case SValue.SBool(b) =>
          if (b)
            machine.returnValue = SValue.SUnit
          else
            abort()
        case _ =>
          throw SErrorCrash("KCheckChoiceGuard", "Expected SBool value.")
      }
    }
  }

  /** unwindToHandler is called when an exception is thrown by the builtin SBThrow or
    * re-thrown by the builtin SBTryHandler. If a catch-handler is found, we initiate
    * execution of the handler code (which might decide to re-throw). Otherwise we call
    * throwUnhandledException to apply the message function to the exception payload,
    * producing a text message.
    */
  private[speedy] def unwindToHandler(machine: Machine, excep: SValue.SAny): Unit = {
    @tailrec def unwind(): Option[KTryCatchHandler] = {
      if (machine.kontDepth() == 0) {
        None
      } else {
        machine.popKont() match {
          case handler: KTryCatchHandler =>
            Some(handler)
          case _: KCloseExercise =>
            machine.withOnLedger("unwindToHandler/KCloseExercise") { onLedger =>
              onLedger.ptx = onLedger.ptx.abortExercises
            }
            unwind()
          case k: KCheckChoiceGuard =>
            // We must abort, because the transaction has failed in a way that is
            // unrecoverable (it depends on the state of an input contract that
            // we may not have the authority to fetch).
            machine.kontStack.clear()
            machine.env.clear()
            machine.envBase = 0
            k.abort()
          case KPreventException(_) =>
            throw SError.SErrorDamlException(
              interpretation.Error.UnhandledException(
                excep.ty,
                excep.value.toUnnormalizedValue,
              )
            )
          case _ =>
            unwind()
        }
      }
    }
    unwind() match {
      case Some(kh) =>
        kh.restore()
        machine.popTempStackToBase()
        machine.ctrl = kh.handler
        machine.pushEnv(excep) // payload on stack where handler expects it
      case None =>
        machine.kontStack.clear()
        machine.env.clear()
        machine.envBase = 0
        throw SErrorDamlException(
          IError.UnhandledException(excep.ty, excep.value.toUnnormalizedValue)
        )
    }
  }

  /** A location frame stores a location annotation found in the AST. */
  final case class KLocation(machine: Machine, location: Location) extends Kont {
    def execute(v: SValue) = {
      machine.returnValue = v
    }
  }

  /** Continuation produced by [[SELabelClsoure]] expressions. This is only
    * used during profiling. Its purpose is to attach a label to closures such
    * that entering the closure can write an "open event" with that label.
    */
  private[speedy] final case class KLabelClosure(machine: Machine, label: Profile.Label)
      extends Kont {
    def execute(v: SValue) = {
      v match {
        case SValue.SPAP(SValue.PClosure(_, expr, closure), args, arity) =>
          machine.returnValue = SValue.SPAP(SValue.PClosure(label, expr, closure), args, arity)
        case _ =>
          machine.returnValue = v
      }
    }
  }

  /** Continuation marking the exit of a closure. This is only used during
    * profiling.
    */
  private[speedy] final case class KLeaveClosure(machine: Machine, label: Profile.Label)
      extends Kont {
    def execute(v: SValue) = {
      machine.profile.addCloseEvent(label)
      machine.returnValue = v
    }
  }

  private[speedy] final case class KPreventException(machine: Machine) extends Kont {
    def execute(v: SValue) =
      machine.returnValue = v
  }

  /** Internal exception thrown when a continuation result needs to be returned.
    *    Or machine execution has reached a final value.
    */
  private[speedy] final case class SpeedyHungry(result: SResult)
      extends RuntimeException
      with NoStackTrace {
    override def toString = s"SpeedyHungry($result)"
  }

  private[speedy] def deriveTransactionSeed(
      submissionSeed: crypto.Hash,
      participant: Ref.ParticipantId,
      submissionTime: Time.Timestamp,
  ): InitialSeeding =
    InitialSeeding.TransactionSeed(
      crypto.Hash.deriveTransactionSeed(submissionSeed, participant, submissionTime)
    )

}
