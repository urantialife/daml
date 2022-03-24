// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Lang {

  type FnName = String

  sealed abstract class Exp
  final case class Num(x: Long) extends Exp
  final case class Add(e1: Exp, e2: Exp) extends Exp
  final case class Sub(e1: Exp, e2: Exp) extends Exp
  final case class Less(e1: Exp, e2: Exp) extends Exp
  final case class IfNot0(e1: Exp, e2: Exp, e3: Exp) extends Exp

  final case class Arg(i: Int) extends Exp
  final case class FnCall(f: FnName, arg: List[Exp]) extends Exp

  case class Program(defs: Map[FnName, Exp], main: Exp)

  type Value = Long

  object Examples {

    def nfibProgram(size: Long): Program = {
      def nfib(e: Exp) = FnCall("nfib", List(e))
      def body(n: Exp): Exp = {
        IfNot0(
          Less(n, Num(2)),
          Num(1),
          Add(Add(nfib(Sub(n, Num(1))), nfib(Sub(n, Num(2)))), Num(1)),
        )
      }
      Program(
        defs = Map("nfib" -> body(Arg(0))),
        main = nfib(Num(size)),
      )
    }

    def tripProgram(i: Long): Program = {
      def loop(step: Exp, acc: Exp, i: Exp) = FnCall("loop", List(step, acc, i))
      val body: Exp = {
        val (step, acc, i) = (Arg(0), Arg(1), Arg(2))
        IfNot0(
          Less(i, Num(1)),
          acc,
          loop(step, Add(acc, step), Sub(i, Num(1))),
        )
      }
      Program(
        defs = Map("loop" -> body),
        main = loop(Num(3L), Num(0L), Num(i)),
      )
    }

  }
}
