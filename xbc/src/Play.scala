// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._

import java.io.FileOutputStream

object Play { // Play with bytecode generation using Asm

  def gen(): Unit = {

    val repo = "/home/nic/daml"
    val className = "MyGenerated"

    val cw: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cw.visit(V1_8, ACC_PUBLIC, className, null, "java/lang/Object", null)
    val mv: MethodVisitor =
      cw.visitMethod(ACC_PUBLIC | ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
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

    val path = repo + "/" + className + ".class"
    val ab: Array[Byte] = cw.toByteArray()
    println(s"Writing: $path, #${ab.length} bytes")
    val f = new FileOutputStream(path)
    f.write(ab)

  }

}
