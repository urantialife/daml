// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Fib {

  def fib(n: Long): Long = {
    if (n < 2) {
      1
    } else {
      fib(n - 1) + fib(n - 2)
    }
  }

}
