// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Speed extends App {

  println(s"Running...");

  val native = Fib.nfib(_)

  val viaLang = (x: Long) => {
    import Lang._
    def program: Program = Examples.nfibProgram(x)
    Interpret.standard(program)
  }

  val pickLangExample: Boolean = true //NICK, select on command line

  val (n, functionUnderTest): (Long, Long => Long) = {
    if (pickLangExample) {
      (33L, viaLang)
    } else {
      (33L, native)
    }
  }

  while (true) {
    val start = System.currentTimeMillis()
    val result = functionUnderTest(n)
    val end = System.currentTimeMillis()
    val duration_ms = end - start
    val duration_s = duration_ms.toFloat / 1000.0
    val speed_us = result / (1000 * duration_ms).toFloat
    println(s"n = $n, result = $result, duration(s) = $duration_s, speed(nfibs/us) = $speed_us")
  }

}
