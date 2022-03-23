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

  final case object Arg1 extends Exp // support functions of 1 arg
  final case class FnCall(f: FnName, arg: Exp) extends Exp

  case class Program(defs: Map[FnName, Exp], main: Exp)

  type Value = Long

  object Examples {

    def nfibProgram(size: Long): Program = {
      val nfib = "nfib"
      def body(n: Exp): Exp = {
        IfNot0(
          Less(n, Num(2)),
          Num(1),
          Add(Add(FnCall(nfib, Sub(n, Num(1))), FnCall(nfib, Sub(n, Num(2)))), Num(1)),
        )
      }
      Program(
        defs = Map(nfib -> body(Arg1)),
        main = FnCall(nfib, Num(size)),
      )
    }

  }
}
