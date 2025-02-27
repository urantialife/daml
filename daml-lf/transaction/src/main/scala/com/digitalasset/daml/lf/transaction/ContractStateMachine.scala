// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package transaction

import com.daml.lf.data.Ref.{Identifier, TypeConName}
import com.daml.lf.transaction.Node.KeyWithMaintainers
import com.daml.lf.transaction.Transaction.{
  DuplicateContractKey,
  InconsistentContractKey,
  KeyCreate,
  KeyInput,
  KeyInputError,
  NegativeKeyLookup,
}
import com.daml.lf.value.Value
import com.daml.lf.value.Value.ContractId

/** Implements a state machine for contracts and their keys while interpreting a Daml-LF command
  * or iterating over a [[com.daml.lf.transaction.HasTxNodes]] in execution order.
  * The contract state machine keeps track of the updates to the [[ContractStateMachine.ActiveLedgerState]]
  * since the beginning of the interpretation or iteration.
  * The interpretation or iteration must call the corresponding handle method for every node
  * on the [[ContractStateMachine.State]].
  *
  * @tparam Nid Type parameter for [[com.daml.lf.transaction.NodeId]]s during interpretation.
  *             Use [[scala.Unit]] for iteration.
  *
  * @see com.daml.lf.transaction.HasTxNodes.contractKeyInputs for an iteration in mode
  *      [[com.daml.lf.transaction.ContractKeyUniquenessMode.Strict]] and
  * @see ContractStateMachineSpec.visitSubtree for iteration in all modes
  */
class ContractStateMachine[Nid](mode: ContractKeyUniquenessMode) {
  import ContractStateMachine._

  def initial: State = State.empty

  /** Invariants:
    * - [[globalKeyInputs]]'s keyset is a superset of [[activeState]].[[ActiveLedgerState.keys]]'s keyset
    *   and of all the [[ActiveLedgerState.keys]]'s keysets in [[rollbackStack]].
    *   (the superset can be strict in case of by-key nodes inside an already completed Rollback scope)
    * - In mode ![[ContractKeyUniquenessMode.Off]],
    *   the keys belonging to the contracts in [[activeState]].[[ActiveLedgerState.consumedBy]]'s
    *   keyset are in [[activeState]].[[ActiveLedgerState.keys]],
    *   and similarly for all [[ActiveLedgerState]]s in [[rollbackStack]].
    *
    * @param locallyCreated
    *   Tracks all contracts created by this transaction including those under a rollback.
    *
    * @param globalKeyInputs
    *   globalKeyInputs contains the key mapping required by Daml Engine to get to the current state.
    *   It contains all keys that have been looked up during interpretation as well as entries for creates
    *   (mapped to KeyCreate).
    *   It can be used to resolve keys during re-interpretation.
    *
    *   In mode [[com.daml.lf.transaction.ContractKeyUniquenessMode.Off]],
    *   a store of key lookups (including fetch-by-key and exercise-by-key,
    *   represented by [[com.daml.lf.transaction.Transaction.NegativeKeyLookup]]
    *   and [[com.daml.lf.transaction.Transaction.KeyActive]])
    *   or keys of created contracts ([[com.daml.lf.transaction.Transaction.KeyCreate]]).
    *
    *   In mode [[com.daml.lf.transaction.ContractKeyUniquenessMode.Strict]],
    *   a store of the contract key states required at the beginning of the transaction with unique contract keys,
    *   where the first node involving a key updates this map.
    *
    *   Grows monotonically. Entries are never overwritten and not reset after a rollback scope is left.
    *
    * @param activeState Summarizes the active state of the partial transaction that was visited so far.
    *                    When a rollback scope is left, this is restored to the state at the beginning of the rollback,
    *                    which is cached in `rollbackStack`.`
    * @param rollbackStack The stack of active states at the beginning of the currently active
    *                      try blocks (interpretation) or Rollback nodes (iteration).
    */
  case class State private (
      locallyCreated: Set[ContractId],
      globalKeyInputs: Map[GlobalKey, KeyInput],
      activeState: ActiveLedgerState[Nid],
      rollbackStack: List[ActiveLedgerState[Nid]],
  ) {

    /** The return value indicates if the given contract is either consumed, inactive, or otherwise
      * - Some(Left(nid)) -- consumed by a specified node-id
      * - Some(Right(())) -- inactive, because the (local) contract creation has been rolled-back
      * - None -- neither consumed or inactive
      */
    def consumedByOrInactive(cid: Value.ContractId): Option[Either[Nid, Unit]] = {
      activeState.consumedBy.get(cid) match {
        case Some(nid) => Some(Left(nid)) // consumed
        case None =>
          if (locallyCreated(cid) && !activeState.locallyCreatedThisTimeline.contains(cid)) {
            Some(Right(())) // inactive
          } else {
            None // neither
          }
      }
    }

    def mode: ContractKeyUniquenessMode = ContractStateMachine.this.mode

    def lookupActiveGlobalKeyInput(key: GlobalKey): Option[ContractStateMachine.KeyMapping] =
      globalKeyInputs.get(key).map {
        case Transaction.KeyActive(cid) if !activeState.consumedBy.contains(cid) =>
          ContractStateMachine.KeyActive(cid)
        case _ => ContractStateMachine.KeyInactive
      }

    /** Visit a create node */
    def handleCreate(node: Node.Create): Either[KeyInputError, State] =
      visitCreate(node.templateId, node.coid, node.key).left.map(Right(_))

    private[lf] def visitCreate(
        templateId: TypeConName,
        contractId: ContractId,
        key: Option[KeyWithMaintainers],
    ): Either[DuplicateContractKey, State] = {
      val me =
        this.copy(
          locallyCreated = locallyCreated + contractId,
          activeState = this.activeState
            .copy(locallyCreatedThisTimeline =
              this.activeState.locallyCreatedThisTimeline + contractId
            ),
        )
      // if we have a contract key being added, include it in the list of
      // active keys
      key match {
        case None => Right(me)
        case Some(kWithM) =>
          val ck = GlobalKey(templateId, kWithM.key)

          // Note (MK) Duplicate key checks in Speedy
          // When run in ContractKeyUniquenessMode.On speedy detects duplicate contract keys errors.
          //
          // Just like for modifying `keys` and `globalKeyInputs` we only consider
          // by-key operations, i.e., lookup, exercise and fetch by key, and creates with a key
          // as well as archives if the key has been brought into scope before.
          //
          // In the end, those checks mean that ledgers with unique-contract-key semantics
          // only have to look at inputs and outputs of the transaction and check for conflicts
          // on that while speedy checks for internal conflicts.
          //
          // We have to consider the following cases for conflicts:
          // 1. Create of a new local contract
          //    1.1. KeyInactive in `keys`. This means we saw an archive so the create is valid.
          //    1.2. KeyActive(_) in `keys`. This can either be local contract or a global contract. Both are an error.
          //    1.3. No entry in `keys` and no entry in `globalKeyInputs`. This is valid. Note that the ledger here will then
          //         have to check when committing that there is no active contract with this key before the transaction.
          //    1.4. No entry in `keys` and `KeyInactive` in `globalKeyInputs`. This is valid. Ledgers need the same check
          //         as for 1.3.
          //    1.5. No entry in `keys` and `KeyActive(_)` in `globalKeyInputs`. This is an error. Note that the case where
          //         the global contract has already been archived falls under 1.2.
          // 2. Global key lookups
          //    2.1. Conflicts with other global contracts cannot arise as we query a key at most once.
          //    2.2. Conflicts with local contracts also cannot arise: A successful create will either
          //         2.2.1: Set `globalKeyInputs` to `KeyInactive`.
          //         2.2.2: Not modify `globalKeyInputs` if there already was an entry.
          //         For both of those cases `globalKeyInputs` already had an entry which means
          //         we would use that as a cached result and not query the ledger.
          val keys = me.activeState.keys
          val conflict = keys.get(ck) match {
            case Some(keyMapping) => keyMapping.isDefined
            case None => lookupActiveGlobalKeyInput(ck).exists(_ != KeyInactive)
          }

          val newKeyInputs =
            if (globalKeyInputs.contains(ck)) globalKeyInputs
            else globalKeyInputs.updated(ck, KeyCreate)
          Either.cond(
            !conflict || mode == ContractKeyUniquenessMode.Off,
            me.copy(
              activeState = me.activeState.copy(keys = keys.updated(ck, KeyActive(contractId))),
              globalKeyInputs = newKeyInputs,
            ),
            DuplicateContractKey(ck),
          )
      }
    }

    def handleExercise(nid: Nid, exe: Node.Exercise): Either[KeyInputError, State] =
      visitExercise(nid, exe.templateId, exe.targetCoid, exe.key, exe.byKey, exe.consuming).left
        .map(Left(_))

    /** Omits the key lookup that are done in [[com.daml.lf.speedy.Compiler.compileChoiceByKey]] for by-bey nodes,
      * which translates to a [[resolveKey]] below.
      * Use [[handleExercise]] when visiting an exercise node during iteration.
      */
    private[lf] def visitExercise(
        nodeId: Nid,
        templateId: TypeConName,
        targetId: ContractId,
        mbKey: Option[KeyWithMaintainers],
        byKey: Boolean,
        consuming: Boolean,
    ): Either[InconsistentContractKey, State] = {
      for {
        state <-
          if (byKey || mode == ContractKeyUniquenessMode.Strict)
            assertKeyMapping(templateId, targetId, mbKey)
          else
            Right(this)
      } yield {
        if (consuming) {
          val consumedState = state.activeState.consume(targetId, nodeId)
          val newActiveState = mbKey match {
            case Some(kWithM) =>
              val gkey = GlobalKey(templateId, kWithM.key)
              val keys = consumedState.keys
              val updatedKeys = keys.updated(gkey, KeyInactive)

              // If the key was brought in scope before, we must update `keys`
              // independently of whether this exercise is by-key because it affects later key lookups.
              if (mode != ContractKeyUniquenessMode.Strict) {
                keys.get(gkey).orElse(state.lookupActiveGlobalKeyInput(gkey)) match {
                  // An archive can only mark a key as inactive
                  // if it was brought into scope before.
                  case Some(KeyActive(cid)) if cid == targetId =>
                    consumedState.copy(keys = updatedKeys)
                  // If the key was not in scope or mapped to a different cid, we don’t change keys. Instead we will do
                  // an activeness check when looking it up later.
                  case _ => consumedState
                }
              } else {
                consumedState.copy(keys = updatedKeys)
              }
            case None => consumedState
          }
          state.copy(activeState = newActiveState)
        } else state
      }
    }

    /**  Must be used to handle lookups iff in [[com.daml.lf.transaction.ContractKeyUniquenessMode.Strict]] mode
      */
    def handleLookup(lookup: Node.LookupByKey): Either[KeyInputError, State] = {
      // If the key has not yet been resolved, we use the resolution from the lookup node,
      // but this only makes sense if `activeState.keys` is updated by every node and not only by by-key nodes.
      if (mode != ContractKeyUniquenessMode.Strict)
        throw new UnsupportedOperationException(
          "handleLookup can only be used if all key nodes are considered"
        )
      visitLookup(lookup.templateId, lookup.key.key, lookup.result, lookup.result).left.map(Left(_))
    }

    /** Must be used to handle lookups iff in [[com.daml.lf.transaction.ContractKeyUniquenessMode.Off]] mode
      * The second argument takes the contract key resolution to be given to the Daml interpreter instead of
      * [[com.daml.lf.transaction.Node.LookupByKey.result]].
      * This is because the iteration might currently be within a rollback scope
      * that has already archived a contract with the key without a by-key operation
      * (and for this reason the archival is not tracked in [[ContractStateMachine.ActiveLedgerState.keys]]),
      * i.e., the lookup resolves to [[scala.None$]] but the correct key input is [[scala.Some$]] for some contract ID
      * and this may matter after the rollback scope is left.
      * Daml Engine will ask the ledger in that case to resolve the lookup and then perform an activeness check
      *  against the result potentially turning it into a negative lookup.
      */
    def handleLookupWith(
        lookup: Node.LookupByKey,
        keyInput: Option[ContractId],
    ): Either[KeyInputError, State] = {
      if (mode != ContractKeyUniquenessMode.Off)
        throw new UnsupportedOperationException(
          "handleLookupWith can only be used if only by-key nodes are considered"
        )
      visitLookup(lookup.templateId, lookup.key.key, keyInput, lookup.result).left.map(Left(_))
    }

    private[lf] def visitLookup(
        templateId: TypeConName,
        key: Value,
        keyInput: Option[ContractId],
        keyResolution: Option[ContractId],
    ): Either[InconsistentContractKey, State] = {
      val gk = GlobalKey.assertBuild(templateId, key)
      val (keyMapping, next) = resolveKey(gk) match {
        case Right(result) => result
        case Left(handle) => handle(keyInput)
      }
      Either.cond(
        keyMapping == keyResolution,
        next,
        InconsistentContractKey(gk),
      )
    }

    private[lf] def resolveKey(
        gkey: GlobalKey
    ): Either[Option[ContractId] => (KeyMapping, State), (KeyMapping, State)] = {
      val keys = activeState.keys
      keys.get(gkey) match {
        case Some(keyMapping) => Right(keyMapping -> this)
        case None =>
          // Check if we have a cached key input.
          lookupActiveGlobalKeyInput(gkey) match {
            case Some(keyMapping) =>
              Right(
                keyMapping -> this.copy(
                  activeState = activeState.copy(keys = keys.updated(gkey, keyMapping))
                )
              )
            case None =>
              // if we cannot find it here, send help, and make sure to update keys after
              // that.
              def handleResult(result: Option[ContractId]): (KeyMapping, State) = {
                // Update key inputs. Create nodes never call this method,
                // so NegativeKeyLookup is the right choice for the global key input.
                val keyInput = result.fold[KeyInput](NegativeKeyLookup)(Transaction.KeyActive)
                val newKeyInputs = globalKeyInputs.updated(gkey, keyInput)

                result match {
                  case Some(cid) if !activeState.consumedBy.contains(cid) =>
                    val active = KeyActive(cid)
                    active -> this.copy(
                      activeState = activeState.copy(keys = keys.updated(gkey, active)),
                      globalKeyInputs = newKeyInputs,
                    )
                  case _ =>
                    KeyInactive -> this.copy(
                      activeState = activeState.copy(
                        keys = keys.updated(gkey, KeyInactive)
                      ),
                      globalKeyInputs = newKeyInputs,
                    )
                }
              }
              Left(handleResult)
          }
      }
    }

    def handleFetch(node: Node.Fetch): Either[KeyInputError, State] =
      visitFetch(node.templateId, node.coid, node.key, node.byKey).left.map(Left(_))

    private[lf] def visitFetch(
        templateId: TypeConName,
        contractId: ContractId,
        key: Option[KeyWithMaintainers],
        byKey: Boolean,
    ): Either[InconsistentContractKey, State] =
      if (byKey || mode == ContractKeyUniquenessMode.Strict)
        assertKeyMapping(templateId, contractId, key)
      else
        Right(this)

    private[this] def assertKeyMapping(
        templateId: Identifier,
        cid: Value.ContractId,
        optKey: Option[Node.KeyWithMaintainers],
    ): Either[InconsistentContractKey, State] = {
      optKey match {
        case None => Right(this)
        case Some(kWithM) =>
          val gk = GlobalKey.assertBuild(templateId, kWithM.key)
          val (keyMapping, next) = resolveKey(gk) match {
            case Right(result) => result
            case Left(handle) => handle(Some(cid))
          }
          // Since keys is defined only where keyInputs is defined, we don't need to update keyInputs.
          Either.cond(keyMapping == KeyActive(cid), next, InconsistentContractKey(gk))
      }
    }

    def handleLeaf(leaf: Node.LeafOnlyAction): Either[KeyInputError, State] =
      leaf match {
        case create: Node.Create => handleCreate(create)
        case fetch: Node.Fetch => handleFetch(fetch)
        case lookup: Node.LookupByKey => handleLookup(lookup)
      }

    /** To be called when interpretation enters a try block or iteration enters a Rollback node
      * Must be matched by [[endRollback]] or [[dropRollback]].
      */
    def beginRollback(): State =
      this.copy(rollbackStack = activeState +: rollbackStack)

    /** To be called when interpretation does insert a Rollback node or iteration leaves a Rollback node.
      * Must be matched by a [[beginRollback]].
      */
    def endRollback(): State = rollbackStack match {
      case Nil => throw new IllegalStateException("Not inside a rollback scope")
      case headState :: tailStack => this.copy(activeState = headState, rollbackStack = tailStack)
    }

    /** To be called if interpretation notices that a try block did not lead to a Rollback node
      * Must be matched by a [[beginRollback]].
      */
    def dropRollback(): State = rollbackStack match {
      case Nil => throw new IllegalStateException("Not inside a rollback scope")
      case _ :: tailStack => this.copy(rollbackStack = tailStack)
    }

    private def withinRollbackScope: Boolean = rollbackStack.nonEmpty

    /** Let `resolver` be a [[KeyResolver]] that can be used during interpretation to obtain a transaction `tx`
      * in mode [[com.daml.lf.transaction.ContractKeyUniquenessMode.Off]],
      * or `Map.empty` in mode [[com.daml.lf.transaction.ContractKeyUniquenessMode.Strict]].
      *
      * Let `this` state be the result of iterating over `tx` up to a node `n` exclusive using `resolver`.
      * Let `substate` be the state obtained after fully iterating over the subtree rooted at `n` starting from [[State.empty]],
      * using the resolver [[projectKeyResolver]](`resolver`)
      * in mode [[com.daml.lf.transaction.ContractKeyUniquenessMode.Off]]
      * or `Map.empty` in mode [[com.daml.lf.transaction.ContractKeyUniquenessMode.Strict]].
      * Then, the returned state is the same as if
      * the iteration over tx continued from `this` state through the whole subtree rooted at `n` using `resolver`.
      *
      * The iteration over the subtree rooted at `n` from `this` using the projected resolver fails
      * if and only if advancing `this` using `substate` does, but the error may be different.
      *
      * @param substate The obtained by iterating over the subtree following `this`.
      *                 Consumed contracts ([[activeState.consumedBy]]) in `this` and `substate` must be disjoint.
      *
      * @see com.daml.lf.transaction.HasTxNodes.contractKeyInputs for an iteration in mode
      *      [[com.daml.lf.transaction.ContractKeyUniquenessMode.Strict]] and
      * @see ContractStateMachineSpec.visitSubtree for iteration in all modes
      */
    def advance(resolver: KeyResolver, substate: State): Either[KeyInputError, State] = {
      require(
        !substate.withinRollbackScope,
        "Cannot lift a state over a substate with unfinished rollback scopes",
      )

      def keyMappingFor(key: GlobalKey): Option[KeyMapping] = {
        this.activeState.keys.get(key).orElse(this.lookupActiveGlobalKeyInput(key))
      }

      // We want consistent key lookups within an action in any contract key mode.
      def consistentGlobalKeyInputs: Either[KeyInputError, Unit] = {
        substate.globalKeyInputs
          .collectFirst {
            case (key, KeyCreate)
                if keyMappingFor(key).exists(_ != KeyInactive) &&
                  mode == ContractKeyUniquenessMode.Strict =>
              Right(DuplicateContractKey(key))
            case (key, NegativeKeyLookup) if keyMappingFor(key).exists(_ != KeyInactive) =>
              Left(InconsistentContractKey(key))
            case (key, Transaction.KeyActive(cid))
                if keyMappingFor(key).exists(km => km != KeyActive(cid)) =>
              Left(InconsistentContractKey(key))
          }
          .toLeft(())
      }

      for {
        _ <- consistentGlobalKeyInputs
      } yield {
        val next = ActiveLedgerState(
          locallyCreatedThisTimeline = this.activeState.locallyCreatedThisTimeline
            .union(substate.activeState.locallyCreatedThisTimeline),
          consumedBy = this.activeState.consumedBy ++ substate.activeState.consumedBy,
          keys = this.activeState.keys.concat(substate.activeState.keys),
        )
        val globalKeyInputs =
          if (mode == ContractKeyUniquenessMode.Strict)
            // In strict mode, `key`'s state is the same at `this` as at the beginning
            // if `key` is not in `this.globalKeyInputs`.
            // So just extend `this.globalKeyInputs` with the new stuff.
            substate.globalKeyInputs ++ this.globalKeyInputs
          else
            substate.globalKeyInputs.foldLeft(this.globalKeyInputs) { case (acc, (key, keyInput)) =>
              if (acc.contains(key)) acc
              else {
                val resolution = keyInput match {
                  case KeyCreate =>
                    // A create brought the contract key in scope without querying a resolver.
                    // So the global key input for `key` does not depend on the resolver.
                    KeyCreate
                  case NegativeKeyLookup =>
                    // A lookup-by-key brought the key in scope. Use the resolver's resolution instead
                    // as the projected resolver's resolution might have been mapped to None.
                    resolver(key).fold(keyInput)(Transaction.KeyActive)
                  case active: Transaction.KeyActive =>
                    active
                }
                acc.updated(key, resolution)
              }
            }

        this.copy(
          locallyCreated = this.locallyCreated.union(substate.locallyCreated),
          globalKeyInputs = globalKeyInputs,
          activeState = next,
        )
      }
    }

    /** @see advance */
    def projectKeyResolver(resolver: KeyResolver): KeyResolver = {
      val keys = activeState.keys
      val consumed = activeState.consumedBy.keySet
      resolver.map { case (key, keyMapping) =>
        val newKeyInput = keys.getOrElse(key, keyMapping.filterNot(consumed.contains))
        key -> newKeyInput
      }
    }
  }

  object State {
    val empty: State = new State(Set.empty, Map.empty, ActiveLedgerState.empty, List.empty)
  }
}

object ContractStateMachine {

  /** Represents the answers for [[com.daml.lf.engine.ResultNeedKey]] requests
    * that may arise during Daml interpretation.
    */
  type KeyResolver = Map[GlobalKey, KeyMapping]

  type KeyMapping = Option[Value.ContractId]

  /** There is no active contract with the given key. */
  val KeyInactive = None

  /** The contract with the given cid is active and has the given key. */
  val KeyActive = Some

  /** Summarizes the updates to the current ledger state by nodes up to now.
    *
    * @param locallyCreatedThisTimeline
    *   Tracks contracts created by this transaction that have not been rolled back. This is a subset of `locallyCreated`.
    *
    * @param consumedBy [[com.daml.lf.value.Value.ContractId]]s of all contracts
    *                   that have been consumed by nodes up to now.
    * @param keys
    *   A local store of the contract keys used for lookups and fetches by keys
    *   (including exercise by key). Each of those operations will be resolved
    *   against this map first. Only if there is no entry in here
    *   (but not if there is an entry mapped to [[KeyInactive]]), will we ask the ledger.
    *
    *   How this map is mutated depends on the [[com.daml.lf.transaction.ContractKeyUniquenessMode]]:
    *
    *   - In mode [[com.daml.lf.transaction.ContractKeyUniquenessMode.Strict]],
    *     the current state reflects the result of applying all nodes (create, fetch, exercise, lookup)
    *     on contracts with a key (regardless of whether they were byKey or not) excluding
    *     nodes under a rollback.
    *
    *   - In mode [[com.daml.lf.transaction.ContractKeyUniquenessMode.Off]]
    *     the following operations mutate this map:
    *     1. fetch-by-key/lookup-by-key/exercise-by-key/create-contract-with-key will insert an
    *        an entry in the map if there wasn’t already one (i.e., if they queried the ledger).
    *     2. ACS mutating operations if the corresponding contract has a key update the entry. Specifically,
    *        2.1. A create will set the corresponding map entry to KeyActive(cid) if the contract has a key.
    *        2.2. A consuming exercise on cid will set the corresponding map entry to KeyInactive
    *             iff we had a KeyActive(cid) entry for the same key before. If not, keys
    *             will not be modified.
    *             Later lookups have an activeness check
    *             that can then set this to KeyInactive if the result of the
    *             lookup was already archived.
    */
  final case class ActiveLedgerState[+Nid](
      locallyCreatedThisTimeline: Set[ContractId],
      consumedBy: Map[ContractId, Nid],
      keys: Map[GlobalKey, KeyMapping],
  ) {
    def consume[Nid2 >: Nid](contractId: ContractId, nodeId: Nid2): ActiveLedgerState[Nid2] =
      this.copy(consumedBy = consumedBy.updated(contractId, nodeId))
  }

  object ActiveLedgerState {
    private val EMPTY: ActiveLedgerState[Nothing] =
      ActiveLedgerState(Set.empty, Map.empty, Map.empty)
    def empty[Nid]: ActiveLedgerState[Nid] = EMPTY
  }
}
