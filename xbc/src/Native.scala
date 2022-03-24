// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

import scala.annotation.tailrec

// Examples writen natively in scala
object Native {

  def nfibRecurive(n: Long): Long = {
    // Not quite the fib function, as here we +1 in the recursive case.
    // So the function result is equal to the number of function calls.
    if (n < 2) {
      1
    } else {
      nfibRecurive(n - 1) + nfibRecurive(n - 2) + 1
    }
  }

  sealed abstract trait Trav
  final case class Down(n: Long) extends Trav
  final case class Up(res: Long) extends Trav

  sealed abstract trait K
  final case object KRet extends K
  final case class KLeft(n: Long, k: K) extends K
  final case class KRight(v: Long, k: K) extends K

  def nfibStackSafe(n: Long): Long = {

    // nesting Trav/K here causes slowdown. why?

    @tailrec
    def nfibLoop(trav: Trav, k: K): Long = {
      trav match {
        case Down(n) =>
          if (n < 2) {
            nfibLoop(Up(1), k)
          } else {
            nfibLoop(Down(n - 1), KLeft(n, k))
          }
        case Up(res) =>
          k match {
            case KRet => res
            case KLeft(n, k) => nfibLoop(Down(n - 2), KRight(res, k))
            case KRight(resL, k) => nfibLoop(Up(resL + res + 1), k)
          }
      }
    }

    nfibLoop(Down(n), KRet)
  }

  @tailrec
  def tripLoop(step: Long, acc: Long, i: Long): Long = {
    if (i < 1) acc
    else {
      tripLoop(step, acc + step, i - 1)
    }
  }

  def trip(i: Long): Long = {
    // compute 3*n using iteration
    val step = 3L
    tripLoop(step, 0L, i)
  }

}
