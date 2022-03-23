// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Fib {

  // Not quite the fib function, as here we +1 in the recursive case.
  // This allows that the result of the function is equal to the number of function calls.
  def nfib(n: Long): Long = {
    if (n < 2) {
      1
    } else {
      nfib(n - 1) + nfib(n - 2) + 1
    }
  }

}
