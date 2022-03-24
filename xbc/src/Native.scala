// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

// Examples writen natively in scala
object Native {

  // Examples should be scalable:
  // - the computational effort should scale exponentially with the input N
  // - the result should be indicative of the computation effort

  def nfib(n: Long): Long = {
    // Not quite the fib function, as here we +1 in the recursive case.
    // This allows that the result of the function is equal to the number of function calls.
    if (n < 2) {
      1
    } else {
      nfib(n - 1) + nfib(n - 2) + 1
    }
  }

  def trix(n: Long): Long = {
    // compute 3*n using iteration, then divide by 3
    //@tailrec //NICK: effects bytecode?
    def loop(step: Long, acc: Long, i: Long): Long = {
      if (i == 0) acc
      else {
        loop(step, acc + step, i - 1)
      }
    }
    val step = 3L
    val i = lpower(2, n) //to be scalable
    loop(step, 0L, i) / step
  }

  def lpower(base: Long, exponent: Long): Long = {
    // power operator defined for longs -- simplistic defintion is all we need
    if (exponent < 0) {
      sys.error(s"\n**lpower, negative exponent: $exponent")
    } else {
      def loop(i: Long): Long = if (i == 0) 1 else base * loop(i - 1)
      loop(exponent)
    }
  }

}
