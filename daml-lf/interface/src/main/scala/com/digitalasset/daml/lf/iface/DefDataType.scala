// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.iface

import scalaz.std.map._
import scalaz.std.option._
import scalaz.std.set._
import scalaz.std.tuple._
import scalaz.syntax.applicative.^
import scalaz.syntax.semigroup._
import scalaz.syntax.traverse._
import scalaz.syntax.std.map._
import scalaz.syntax.std.option._
import scalaz.{Applicative, Bifunctor, Bitraverse, Bifoldable, Foldable, Functor, Monoid, Traverse}
import scalaz.Tags.FirstVal
import java.{util => j}

import com.daml.lf.data.ImmArray.ImmArraySeq
import com.daml.lf.data.Ref
import com.daml.nonempty.NonEmpty

import scala.jdk.CollectionConverters._

case class DefDataType[+RF, +VF](typeVars: ImmArraySeq[Ref.Name], dataType: DataType[RF, VF]) {
  def bimap[C, D](f: RF => C, g: VF => D): DefDataType[C, D] =
    Bifunctor[DefDataType].bimap(this)(f, g)

  def getTypeVars: j.List[_ <: String] = typeVars.asJava
}

object DefDataType {

  /** Alias for application to [[Type]]. Note that FWT stands for "Field with
    * type", because before we parametrized over both the field and the type,
    * while now we only parametrize over the type.
    */
  type FWT = DefDataType[Type, Type]

  implicit val `DDT bitraverse`: Bitraverse[DefDataType] =
    new Bitraverse[DefDataType] with Bifoldable.FromBifoldMap[DefDataType] {

      override def bimap[A, B, C, D](
          fab: DefDataType[A, B]
      )(f: A => C, g: B => D): DefDataType[C, D] = {
        DefDataType(fab.typeVars, Bifunctor[DataType].bimap(fab.dataType)(f, g))
      }

      override def bifoldMap[A, B, M: Monoid](fab: DefDataType[A, B])(f: A => M)(g: B => M): M = {
        Bifoldable[DataType].bifoldMap(fab.dataType)(f)(g)
      }

      override def bitraverseImpl[G[_]: Applicative, A, B, C, D](
          fab: DefDataType[A, B]
      )(f: A => G[C], g: B => G[D]): G[DefDataType[C, D]] = {
        Applicative[G].map(Bitraverse[DataType].bitraverse(fab.dataType)(f)(g))(dataTyp =>
          DefDataType(fab.typeVars, dataTyp)
        )
      }
    }
}

sealed trait DataType[+RT, +VT] extends Product with Serializable {
  def bimap[C, D](f: RT => C, g: VT => D): DataType[C, D] =
    Bifunctor[DataType].bimap(this)(f, g)
}

object DataType {

  /** Alias for application to [[Type]]. Note that FWT stands for "Field with
    * type", because before we parametrized over both the field and the type,
    * while now we only parametrize over the type.
    */
  type FWT = DataType[Type, Type]

  // While this instance appears to overlap the subclasses' traversals,
  // naturality holds with respect to those instances and this one, so there is
  // no risk of confusion.
  implicit val `DT bitraverse`: Bitraverse[DataType] =
    new Bitraverse[DataType] with Bifoldable.FromBifoldMap[DataType] {

      override def bimap[A, B, C, D](fab: DataType[A, B])(f: A => C, g: B => D): DataType[C, D] =
        fab match {
          case r @ Record(_) =>
            Functor[Record].map(r)(f).widen
          case v @ Variant(_) =>
            Functor[Variant].map(v)(g).widen
          case e @ Enum(_) =>
            e
        }

      override def bifoldMap[A, B, M: Monoid](fab: DataType[A, B])(f: A => M)(g: B => M): M =
        fab match {
          case r @ Record(_) =>
            Foldable[Record].foldMap(r)(f)
          case v @ Variant(_) =>
            Foldable[Variant].foldMap(v)(g)
          case Enum(_) => {
            val m = implicitly[Monoid[M]]
            m.zero
          }
        }

      override def bitraverseImpl[G[_]: Applicative, A, B, C, D](
          fab: DataType[A, B]
      )(f: A => G[C], g: B => G[D]): G[DataType[C, D]] =
        fab match {
          case r @ Record(_) =>
            Traverse[Record].traverse(r)(f).widen
          case v @ Variant(_) =>
            Traverse[Variant].traverse(v)(g).widen
          case e @ Enum(_) =>
            Applicative[G].pure(e)
        }
    }

  sealed trait GetFields[+A] {
    def fields: ImmArraySeq[(Ref.Name, A)]
    final def getFields: j.List[_ <: (String, A)] = fields.asJava
  }
}

// Record TypeDecl`s have an object generated for them in their own file
final case class Record[+RT](fields: ImmArraySeq[(Ref.Name, RT)])
    extends DataType[RT, Nothing]
    with DataType.GetFields[RT] {

  /** Widen to DataType, in Java. */
  def asDataType[PRT >: RT, VT]: DataType[PRT, VT] = this
}

object Record extends FWTLike[Record] {
  implicit val `R traverse`: Traverse[Record] =
    new Traverse[Record] with Foldable.FromFoldMap[Record] {

      override def map[A, B](fa: Record[A])(f: A => B): Record[B] =
        Record(fa.fields map (_ map f))

      override def foldMap[A, B: Monoid](fa: Record[A])(f: A => B): B =
        fa.fields foldMap { case (_, a) => f(a) }

      override def traverseImpl[G[_]: Applicative, A, B](fa: Record[A])(
          f: A => G[B]
      ): G[Record[B]] =
        Applicative[G].map(fa.fields traverse (_ traverse f))(bs => fa.copy(fields = bs))
    }
}

// Variant TypeDecl`s have an object generated for them in their own file
final case class Variant[+VT](fields: ImmArraySeq[(Ref.Name, VT)])
    extends DataType[Nothing, VT]
    with DataType.GetFields[VT] {

  /** Widen to DataType, in Java. */
  def asDataType[RT, PVT >: VT]: DataType[RT, PVT] = this
}

object Variant extends FWTLike[Variant] {
  implicit val `V traverse`: Traverse[Variant] =
    new Traverse[Variant] with Foldable.FromFoldMap[Variant] {

      override def map[A, B](fa: Variant[A])(f: A => B): Variant[B] =
        Variant(fa.fields map (_ map f))

      override def foldMap[A, B: Monoid](fa: Variant[A])(f: A => B): B =
        fa.fields foldMap { case (_, a) => f(a) }

      override def traverseImpl[G[_]: Applicative, A, B](fa: Variant[A])(
          f: A => G[B]
      ): G[Variant[B]] =
        Applicative[G].map(fa.fields traverse (_ traverse f))(bs => fa.copy(fields = bs))
    }
}

final case class Enum(constructors: ImmArraySeq[Ref.Name]) extends DataType[Nothing, Nothing] {

  /** Widen to DataType, in Java. */
  def asDataType[RT, PVT]: DataType[RT, PVT] = this
}

final case class DefTemplate[+Ty](
    tChoices: TemplateChoices[Ty],
    key: Option[Ty],
    implementedInterfaces: Seq[Ref.TypeConName],
) {
  def map[B](f: Ty => B): DefTemplate[B] = Functor[DefTemplate].map(this)(f)

  @deprecated("use tChoices.directChoices or tChoices.resolvedChoices instead", since = "2.3.0")
  private[daml] def choices = tChoices.directChoices

  /** Remove choices from `unresolvedInheritedChoices` and add to `choices`
    * given the `astInterfaces` from an [[EnvironmentInterface]].  If the result
    * has any `unresolvedInheritedChoices` left, these choices were not found.
    */
  def resolveChoices[O >: Ty](
      astInterfaces: PartialFunction[Ref.TypeConName, DefInterface[O]]
  ): Either[TemplateChoices.ResolveError[DefTemplate[O]], DefTemplate[O]] = {
    import scalaz.std.either._
    import scalaz.syntax.bifunctor._
    tChoices resolveChoices astInterfaces bimap (_.map(r => copy(tChoices = r)), r =>
      copy(tChoices = r))
  }

  def getKey: j.Optional[_ <: Ty] = toOptional(key)
}

object DefTemplate {
  type FWT = DefTemplate[Type]

  implicit val `TemplateDecl traverse`: Traverse[DefTemplate] =
    new Traverse[DefTemplate] with Foldable.FromFoldMap[DefTemplate] {
      override def foldMap[A, B: Monoid](fa: DefTemplate[A])(f: A => B): B =
        (fa.tChoices foldMap f) |+| (fa.key foldMap f)

      override def traverseImpl[G[_]: Applicative, A, B](
          fab: DefTemplate[A]
      )(f: A => G[B]): G[DefTemplate[B]] =
        ^(fab.tChoices traverse f, fab.key traverse f) { (choices, key) =>
          fab.copy(tChoices = choices, key = key)
        }
    }

  private[daml] val Empty: DefTemplate[Nothing] =
    DefTemplate(TemplateChoices.Resolved(Map.empty), None, Seq.empty)
}

/** Choices in a [[DefTemplate]]. */
sealed abstract class TemplateChoices[+Ty] extends Product with Serializable {
  import TemplateChoices.{Resolved, Unresolved, ResolveError, directAsResolved, logger}

  /** Choices defined directly on the template */
  def directChoices: Map[Ref.ChoiceName, TemplateChoice[Ty]]

  /** Choices defined on the template, or on resolved implemented interfaces if
    * resolved
    */
  def resolvedChoices
      : Map[Ref.ChoiceName, NonEmpty[Map[Option[Ref.TypeConName], TemplateChoice[Ty]]]]

  /** A shim function to delay porting a component to overloaded choices.
    * Discards essential data, so not a substitute for a proper port.
    * TODO (#13974) delete when there are no more callers
    */
  private[daml] def assumeNoOverloadedChoices(
      githubIssue: Int
  ): Map[Ref.ChoiceName, TemplateChoice[Ty]] = this match {
    case Unresolved(directChoices, _) => directChoices
    case Resolved(resolvedChoices) =>
      resolvedChoices.transform { (choiceName, overloads) =>
        if (overloads.sizeIs == 1) overloads.head1._2
        else
          overloads
            .get(None)
            .cata(
              { directChoice =>
                logger.warn(s"discarded inherited choices for $choiceName, see #$githubIssue")
                directChoice
              }, {
                val (Some(randomKey), randomChoice) = overloads.head1
                logger.warn(
                  s"selected $randomKey-inherited choice but discarded others for $choiceName, see #$githubIssue"
                )
                randomChoice
              },
            )
      }
  }

  final def getDirectChoices: j.Map[Ref.ChoiceName, _ <: TemplateChoice[Ty]] =
    directChoices.asJava

  final def getResolvedChoices
      : j.Map[Ref.ChoiceName, _ <: j.Map[j.Optional[Ref.TypeConName], _ <: TemplateChoice[Ty]]] =
    resolvedChoices.transform((_, m) => m.forgetNE.mapKeys(toOptional).asJava).asJava

  /** Coerce to [[Resolved]] based on the environment `astInterfaces`, or fail
    * with the choices that could not be resolved.
    */
  private[iface] def resolveChoices[O >: Ty](
      astInterfaces: PartialFunction[Ref.TypeConName, DefInterface[O]]
  ): Either[ResolveError[Resolved[O]], Resolved[O]] = this match {
    case Unresolved(direct, unresolved) =>
      val getAstInterface = astInterfaces.lift
      type ResolutionResult[C] =
        (Set[Ref.TypeConName], Map[Ref.ChoiceName, NonEmpty[Map[Option[Ref.TypeConName], C]]])
      val (missing, resolved): ResolutionResult[TemplateChoice[O]] =
        FirstVal.unsubst[ResolutionResult, TemplateChoice[O]](
          unresolved.forgetNE
            .foldMap { tcn =>
              getAstInterface(tcn).cata(
                { astIf =>
                  val tcnResolved =
                    astIf.choices.transform((_, tc) => NonEmpty(Map, some(tcn) -> tc))
                  FirstVal.subst[ResolutionResult, TemplateChoice[O]](
                    Set.empty[Ref.TypeConName],
                    tcnResolved,
                  )
                },
                (Set(tcn), Map.empty): ResolutionResult[Nothing],
              )
            }
        )
      val rChoices = Resolved(resolved.unionWith(directAsResolved(direct))(_ ++ _))
      missing match {
        case NonEmpty(missing) => Left(ResolveError(missing, rChoices))
        case _ => Right(rChoices)
      }
    case r @ Resolved(_) => Right(r)
  }
}

object TemplateChoices {
  private val logger = com.typesafe.scalalogging.Logger(getClass)

  final case class ResolveError[+Partial](
      missingInterfaces: NonEmpty[Set[Ref.TypeConName]],
      partialResolution: Partial,
  ) {
    private[iface] def describeError: String =
      missingInterfaces.mkString(", ")

    private[iface] def map[B](f: Partial => B): ResolveError[B] =
      copy(partialResolution = f(partialResolution))
  }

  private[iface] final case class Unresolved[+Ty](
      directChoices: Map[Ref.ChoiceName, TemplateChoice[Ty]],
      unresolvedChoiceSources: NonEmpty[Set[Ref.TypeConName]],
  ) extends TemplateChoices[Ty] {
    override def resolvedChoices =
      directAsResolved(directChoices)
  }

  private[TemplateChoices] def directAsResolved[Ty](
      directChoices: Map[Ref.ChoiceName, TemplateChoice[Ty]]
  ) =
    directChoices transform ((_, c) => NonEmpty(Map, (none[Ref.TypeConName], c)))

  private[iface] final case class Resolved[+Ty](
      resolvedChoices: Map[Ref.ChoiceName, NonEmpty[
        Map[Option[Ref.TypeConName], TemplateChoice[Ty]]
      ]]
  ) extends TemplateChoices[Ty] {
    override def directChoices = resolvedChoices collect (Function unlift { case (cn, m) =>
      m get None map ((cn, _))
    })
  }

  object Resolved {
    private[daml] def fromDirect[Ty](directChoices: Map[Ref.ChoiceName, TemplateChoice[Ty]]) =
      Resolved(directAsResolved(directChoices))
  }

  implicit val `TemplateChoices traverse`: Traverse[TemplateChoices] = new Traverse[TemplateChoices]
    with Foldable.FromFoldMap[TemplateChoices] {
    override def foldMap[A, B: Monoid](fa: TemplateChoices[A])(f: A => B): B = fa match {
      case Unresolved(direct, _) => direct foldMap (_ foldMap f)
      case Resolved(resolved) => resolved foldMap (_.toNEF foldMap (_ foldMap f))
    }

    override def traverseImpl[G[_]: Applicative, A, B](
        fa: TemplateChoices[A]
    )(f: A => G[B]): G[TemplateChoices[B]] = fa match {
      case u @ Unresolved(_, _) =>
        u.directChoices traverse (_ traverse f) map (dc => u.copy(directChoices = dc))
      case Resolved(r) => r traverse (_.toNEF traverse (_ traverse f)) map (Resolved(_))
    }
  }
}

final case class TemplateChoice[+Ty](param: Ty, consuming: Boolean, returnType: Ty) {
  def map[C](f: Ty => C): TemplateChoice[C] =
    Functor[TemplateChoice].map(this)(f)
}

object TemplateChoice {
  type FWT = TemplateChoice[Type]

  implicit val `Choice traverse`: Traverse[TemplateChoice] = new Traverse[TemplateChoice] {
    override def traverseImpl[G[_]: Applicative, A, B](
        fa: TemplateChoice[A]
    )(f: A => G[B]): G[TemplateChoice[B]] =
      ^(f(fa.param), f(fa.returnType)) { (param, returnType) =>
        fa.copy(param = param, returnType = returnType)
      }
  }
}

final case class DefInterface[+Ty](choices: Map[Ref.ChoiceName, TemplateChoice[Ty]]) {
  def getChoices: j.Map[Ref.ChoiceName, _ <: TemplateChoice[Ty]] =
    choices.asJava
}

object DefInterface extends FWTLike[DefInterface] {

  implicit val `InterfaceDecl fold`: Foldable[DefInterface] =
    new Foldable.FromFoldMap[DefInterface] {
      override def foldMap[A, B: Monoid](fa: DefInterface[A])(f: A => B): B =
        fa.choices.foldMap(_ foldMap f)
    }

}

/** Add aliases to companions. */
sealed abstract class FWTLike[F[+_]] {

  /** Alias for application to [[Type]]. Note that FWT stands for "Field with
    * type", because before we parametrized over both the field and the type,
    * while now we only parametrize over the type.
    */
  type FWT = F[Type]
}
