// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Speed extends App {

  println(s"Running...");

  val n = 40L

  while (true) {
    val start = System.currentTimeMillis()
    val result = Fib.nfib(n)
    val end = System.currentTimeMillis()
    val duration_ms = end - start
    val duration_s = duration_ms.toFloat / 1000.0
    val speed_us = result / (1000 * duration_ms).toFloat
    println(s"n = $n, result = $result, duration(s) = $duration_s, speed(nfibs/us) = $speed_us")
  }

}
