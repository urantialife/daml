// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Speed extends App {

  sealed trait Conf
  object Conf {
    final case class FixedN(group: String, version: String, n: Long) extends Conf
    final case class IncreasingN(group: String, version: String) extends Conf
  }

  def conf: Conf = {
    val defaultGroup: String = "nfib"
    val defaultVersion: String = "native"
    def xv(v: String) = v match {
      case "n" => "native"
      case "i" => "interpreter"
      case s => s
    }
    args.toList match {
      case Nil =>
        Conf.IncreasingN(defaultGroup, defaultVersion)
      case group :: Nil =>
        Conf.IncreasingN(group, defaultVersion)
      case group :: version :: Nil =>
        Conf.IncreasingN(group, xv(version))
      case group :: version :: n :: Nil =>
        Conf.FixedN(group, xv(version), n.toLong)
      case _ =>
        sys.error(s"\n**unexpected command line: $args")
    }
  }

  println(s"Running: $conf...");

  type FUT = (Long => Long) // function under test

  val nfib_n: FUT = Native.nfib(_)
  val nfib_i: FUT = (x: Long) => {
    import Lang._
    def program: Program = Examples.nfibProgram(x)
    Interpret.standard(program)
  }
  val trix_n: FUT = Native.trix(_)

  // map of groups of FUTs
  def m: Map[String, List[(String, FUT)]] = Map(
    "nfib" -> List(
      "native" -> nfib_n,
      "interpreter" -> nfib_i,
    ),
    "trix" -> List(
      "native" -> trix_n
    ),
  )

  def getFUT(group: String, version: String): FUT = {
    m.get(group) match {
      case None => sys.error(s"\n**unknown group: $group")
      case Some(g) =>
        g.toMap.get(version) match {
          case None => sys.error(s"\n**unknown version: $version")
          case Some(f) => f
        }
    }
  }

  conf match {
    case Conf.FixedN(group, version, n) =>
      printHeader()
      val setup = Setup(group, version, n)
      while (true) {
        val outcome = runTest(setup)
        printOutcomeLine(outcome)
      }

    case Conf.IncreasingN(group, version) =>
      printHeader()
      var n: Long = 20L
      while (true) {
        val setup = Setup(group, version, n)
        val outcome = runTest(setup)
        printOutcomeLine(outcome)
        n += 1
      }
  }

  case class Setup(group: String, version: String, n: Long)

  case class Outcome(setup: Setup, res: Long, dur_s: Double, speed: Float)

  def runTest(setup: Setup): Outcome = {
    setup match {
      case Setup(group, version, n) =>
        val functionUnderTest = getFUT(group, version)
        val start = System.currentTimeMillis()
        val res = functionUnderTest(n)
        val end = System.currentTimeMillis()
        val dur_ms = end - start
        val dur_s = dur_ms.toFloat / 1000.0
        val speed = res / (1000 * dur_ms).toFloat
        Outcome(setup, res, dur_s, speed)
    }
  }

  def printHeader() = {
    println(s"scale result  duration  speed      name")
    println(s"N   #ops        secs    ops/us     group-version")
    println(s"--------------------------------------------------")
  }

  def printOutcomeLine(outcome: Outcome) = {

    def pad(s: String, max: Int): String = {
      val ss = s.size
      val n = if (ss < max) max - ss else 0
      val padding: String = List.fill(n)(' ').mkString
      s + padding
    }

    outcome match {
      case Outcome(setup, res, dur_s, speed) =>
        setup match {
          case Setup(group, version, n) =>
            println(
              s"${pad(s"$n", 4)}${pad(s"$res", 12)}${pad(f"$dur_s%.2f", 8)}${pad(f"$speed%.2f", 11)}$group-$version"
            )
        }
    }
  }

}
