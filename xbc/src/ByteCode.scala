// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

import java.io.FileOutputStream

final class ByteCode(className: String, methodName: String, bytes: Array[Byte]) {

  def dump() = {
    val base = "/tmp/"
    val path = base + className + ".class"
    val f = new FileOutputStream(path)
    println(s"generating $path, #${bytes.length} bytes")
    f.write(bytes)
  }

  // load and invoke some (perhaps dynamically generated) bytecode
  case object MyClassLoader extends ClassLoader {
    override def findClass(name: String): Class[_] = {
      defineClass(name, bytes, 0, bytes.length)
    }
  }

  def run0(): Unit = { // run bytecode with 0 args
    val aClass = MyClassLoader.loadClass(className)
    val method = aClass.getMethod(methodName)
    val _ = method.invoke(null)
    ()
  }

  def run1(arg1: Long): Unit = { // run bytecode with 1 long arg
    val aClass = MyClassLoader.loadClass(className)
    val longType: Class[_] = classOf[Long]
    val method = aClass.getMethod(methodName, longType)
    val _ = method.invoke(null, arg1)
    ()
  }

  def run2(arg1: Long, arg2: Long): Unit = { // run bytecode with 2 long args
    val aClass = MyClassLoader.loadClass(className)
    val longType: Class[_] = classOf[Long]
    val method = aClass.getMethod(methodName, longType, longType)
    val _ = method.invoke(null, arg1, arg2)
    ()
  }

}
