// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Interpret {

  import Lang._

  type Value = Long

  def standard(program: Program): Value = {
    program match {
      case Program(defs, main) =>
        def evalFrame(theArgValue: Option[Value], body: Exp): Value = {

          def eval(exp: Exp): Value =
            exp match {
              case Num(x) => x
              case Add(e1, e2) => eval(e1) + eval(e2)
              case Sub(e1, e2) => eval(e1) - eval(e2)
              case Less(e1, e2) => if (eval(e1) < eval(e2)) 1 else 0
              case IfNot0(e1, e2, e3) => if (eval(e1) != 0) eval(e2) else eval(e3)
              case Arg1 =>
                theArgValue match {
                  case None => sys.error("Arg1,None")
                  case Some(v) => v
                }
              case FnCall(fnName, arg) =>
                defs.get(fnName) match {
                  case None => sys.error(s"FnCall: $fnName")
                  case Some(body) =>
                    val v = eval(arg)
                    evalFrame(Some(v), body)
                }
            }

          eval(body)
        }

        evalFrame(None, main)
    }
  }

}
