// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Interpret {

  import Lang._

  type Value = Long

  def standard(program: Program): Value = {
    program match {
      case Program(defs, main) =>
        def evalFrame(actuals: Vector[Value], body: Exp): Value = {

          def eval(exp: Exp): Value =
            exp match {
              case Num(x) => x
              case Add(e1, e2) => eval(e1) + eval(e2)
              case Sub(e1, e2) => eval(e1) - eval(e2)
              case Less(e1, e2) => if (eval(e1) < eval(e2)) 1 else 0
              case IfNot0(e1, e2, e3) => if (eval(e1) != 0) eval(e2) else eval(e3)
              case Arg(i) => actuals(i)
              case FnCall(fnName, args) =>
                defs.get(fnName) match {
                  case None => sys.error(s"FnCall: $fnName")
                  case Some(body) =>
                    val vs = args.map(eval(_))
                    evalFrame(vs, body)
                }
            }

          eval(body)
        }

        evalFrame(Vector(), main)
    }
  }

}
