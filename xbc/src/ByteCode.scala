// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

final class ByteCode(className: String, methodName: String, bytes: Array[Byte]) {
  def run(): Unit = {
    // load and invoke some (perhaps dynamically generated) bytecode
    case object MyClassLoader extends ClassLoader {
      override def findClass(name: String): Class[_] = {
        defineClass(name, bytes, 0, bytes.length)
      }
    }
    val aClass = MyClassLoader.loadClass(className)
    val method = aClass.getMethod(methodName)
    val _ = method.invoke(null)
    ()
  }
}
