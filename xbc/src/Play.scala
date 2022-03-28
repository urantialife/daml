// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

import org.objectweb.asm._
import org.objectweb.asm.Opcodes._

object Play { // Play with bytecode generation using Asm

  def makeCodeToPrintMessage(message: String): ByteCode = {

    val className = "MyClass"
    val methodName = "go"

    val cw: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cw.visit(V1_8, ACC_PUBLIC, className, null, "java/lang/Object", null)
    val mv: MethodVisitor =
      cw.visitMethod(ACC_PUBLIC | ACC_STATIC, methodName, "()V", null, null)
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    mv.visitLdcInsn(message)
    mv.visitMethodInsn(
      INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      "(Ljava/lang/String;)V",
      false,
    )
    mv.visitInsn(RETURN)
    mv.visitMaxs(-1, -1)

    val bytes = cw.toByteArray()
    new ByteCode(className, methodName, bytes)
  }

}
