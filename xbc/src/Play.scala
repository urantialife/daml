// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._

class MyClassLoader extends ClassLoader { //NICK, take byte array as class arg?

  override def findClass(name: String): Class[_] = {
    println(s"MyClassLoader.findClass: $name ...")
    def ab: Array[Byte] = Play.makeByteArray()
    defineClass(name, ab, 0, ab.length)
  }
}

object Play { // Play with bytecode generation using Asm

  val theClassName = "MyClass"
  val theMethodName = "go"

  def run(): Unit = {
    val loader = new MyClassLoader()
    val aClass = loader.loadClass(theClassName)
    val method = aClass.getMethod(theMethodName)
    println("calling...");
    val _ = method.invoke(null);
    println("calling...done");
  }

  def makeByteArray(): Array[Byte] = {
    val cw: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cw.visit(V1_8, ACC_PUBLIC, theClassName, null, "java/lang/Object", null)
    val mv: MethodVisitor =
      cw.visitMethod(ACC_PUBLIC | ACC_STATIC, theMethodName, "()V", null, null)
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    mv.visitLdcInsn("Hello, here is my message!")
    mv.visitMethodInsn(
      INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      "(Ljava/lang/String;)V",
      false,
    )
    mv.visitInsn(RETURN)
    mv.visitMaxs(-1, -1)
    val ab: Array[Byte] = cw.toByteArray()
    ab
  }

}
