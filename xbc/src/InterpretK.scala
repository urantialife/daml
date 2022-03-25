// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

import scala.annotation.tailrec

object InterpretK {

  import Lang._

  type Value = Long

  sealed abstract trait Trav
  final case class Down(actuals: Vector[Value], exp: Exp) extends Trav
  final case class Up(res: Value) extends Trav

  sealed abstract trait K
  final case object KRet extends K
  final case class KAdd1(actuals: Vector[Value], e2: Exp, k: K) extends K
  final case class KAdd2(res1: Value, k: K) extends K
  final case class KSub1(actuals: Vector[Value], e2: Exp, k: K) extends K
  final case class KSub2(res1: Value, k: K) extends K
  final case class KLess1(actuals: Vector[Value], e2: Exp, k: K) extends K
  final case class KLess2(res1: Value, k: K) extends K
  final case class KIf(actuals: Vector[Value], e2: Exp, e3: Exp, k: K) extends K
  final case class KMoreArgs(
      actuals: Vector[Value],
      results: List[Value],
      exps: List[Exp],
      body: Exp,
      k: K,
  ) extends K

  def cps(program: Program): Value = {
    program match {
      case Program(defs, main) =>
        val _ = defs

        @tailrec
        def loop(trav: Trav, k: K): Value = {
          trav match {
            case Down(actuals, exp) =>
              exp match {
                case Num(x) => loop(Up(x), k)
                case Add(e1, e2) => loop(Down(actuals, e1), KAdd1(actuals, e2, k))
                case Sub(e1, e2) => loop(Down(actuals, e1), KSub1(actuals, e2, k))
                case Less(e1, e2) => loop(Down(actuals, e1), KLess1(actuals, e2, k))
                case Arg(i) => loop(Up(actuals(i)), k)
                case IfNot0(e1, e2, e3) => loop(Down(actuals, e1), KIf(actuals, e2, e3, k))
                case FnCall(fnName, args) =>
                  defs.get(fnName) match {
                    case None => sys.error(s"FnCall: $fnName")
                    case Some(body) =>
                      args match {
                        case Nil =>
                          loop(Down(Vector(), body), k)
                        case arg :: args =>
                          loop(Down(actuals, arg), KMoreArgs(actuals, Nil, args, body, k))
                      }
                  }
              }
            case Up(res) =>
              k match {
                case KRet => res
                case KAdd1(actuals, e2, k) => loop(Down(actuals, e2), KAdd2(res, k))
                case KAdd2(res1, k) => loop(Up(res1 + res), k)
                case KSub1(actuals, e2, k) => loop(Down(actuals, e2), KSub2(res, k))
                case KSub2(res1, k) => loop(Up(res1 - res), k)
                case KLess1(actuals, e2, k) => loop(Down(actuals, e2), KLess2(res, k))
                case KLess2(res1, k) => loop(Up(if (res1 < res) 1 else 0), k)
                case KIf(actuals, e2, e3, k) =>
                  if (res != 0) loop(Down(actuals, e2), k) else loop(Down(actuals, e3), k)
                case KMoreArgs(actuals, results0, args, body, k) =>
                  val results = res :: results0
                  args match {
                    case Nil =>
                      val vs = results.reverse.toVector
                      loop(Down(vs, body), k)
                    case arg :: args =>
                      loop(Down(actuals, arg), KMoreArgs(actuals, results, args, body, k))
                  }
              }
          }
        }

        loop(Down(Vector(), main), KRet)
    }

  }

}
