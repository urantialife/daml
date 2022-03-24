// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Speed extends App {

  sealed trait Conf
  object Conf {
    final case class FixedN(group: String, version: String, n: Long) extends Conf
    final case class IncreasingN(group: String, version: String) extends Conf
    final case class WholeGroup(group: String) extends Conf
  }

  def conf: Conf = {
    val defaultGroup: String = "nfib"
    def xv(v: String) = v match {
      case "n" => "native"
      case "i" => "interpreter"
      case s => s
    }
    args.toList match {
      case Nil =>
        Conf.WholeGroup(defaultGroup)
      case group :: Nil =>
        Conf.WholeGroup(group)
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

  // Examples should be scalable:
  // - the computational effort should scale exponentially with the input N
  // - the result should be indicative of the computation effort

  val nfib_n: FUT = Native.nfib(_)

  val nfib_i: FUT = (x: Long) => {
    import Lang._
    def program: Program = Examples.nfibProgram(x)
    Interpret.standard(program)
  }

  val trip_n: FUT = {
    def lpower(base: Long, exponent: Long): Long = {
      if (exponent < 0) {
        sys.error(s"\n**lpower, negative exponent: $exponent")
      } else {
        def powerLoop(i: Long): Long = if (i == 0) 1 else base * powerLoop(i - 1)
        powerLoop(exponent)
      }
    }
    def scalable_trip(n: Long): Long = {
      val i = lpower(2, n)
      Native.trip(i)
    }
    scalable_trip(_) / 3L
  }

  // map of groups of FUTs
  def m: Map[String, List[(String, FUT)]] = Map(
    "nfib" -> List(
      "interpreter" -> nfib_i,
      "native" -> nfib_n,
    ),
    "trip" -> List(
      "native" -> trip_n
    ),
  )

  def getVersions(group: String): List[String] = {
    m.get(group) match {
      case None => sys.error(s"\n**unknown group: $group")
      case Some(g) => g.map(_._1)
    }
  }

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
        printOutcomeLine(outcome, 1.0)
      }

    case Conf.IncreasingN(group, version) =>
      printHeader()
      var n: Long = 27L
      while (true) {
        val setup = Setup(group, version, n)
        val outcome = runTest(setup)
        printOutcomeLine(outcome, 1.0)
        n += 1
      }

    case Conf.WholeGroup(group) =>
      printHeader()

      def runVersion(version: String): Outcome = {
        def loop(n: Long): Outcome = {
          val setup = Setup(group, version, n)
          val outcome = runTest(setup)
          if (outcome.dur_s > 0.3) {
            outcome
          } else {
            loop(n + 1)
          }
        }
        loop(20L)
      }

      getVersions(group) match {
        case Nil =>
          sys.error(s"\n**no versions for group: $group")
        case leadVersion :: versions =>
          val leadOutcome = runVersion(leadVersion)
          printOutcomeLine(leadOutcome, 1.0)
          versions.foreach { version =>
            val outcome = runVersion(version)
            val relative: Double = outcome.speed / leadOutcome.speed
            printOutcomeLine(outcome, relative)
          }
      }
  }

  case class Setup(group: String, version: String, n: Long)

  case class Outcome(setup: Setup, res: Long, dur_s: Double, speed: Double)

  def runTest(setup: Setup): Outcome = {
    setup match {
      case Setup(group, version, n) =>
        val functionUnderTest = getFUT(group, version)
        val start = System.currentTimeMillis()
        val res = functionUnderTest(n)
        val end = System.currentTimeMillis()
        val dur_ms = end - start
        val dur_s = dur_ms.toFloat / 1000.0
        val speed = res / (1000 * dur_ms).toDouble
        Outcome(setup, res, dur_s, speed)
    }
  }

  def printHeader() = {
    println(s"size result   duration  speed     relative  name")
    println(s"N   #ops         secs   ops/us    speedup   group-version")
    println(s"----------------------------------------------------------")
  }

  def printOutcomeLine(outcome: Outcome, relative: Double) = {

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
              s"${pad(s"$n", 3)} ${pad(s"$res", 12)} ${pad(f"$dur_s%.2f", 6)} ${pad(
                f"$speed%.2f",
                9,
              )} x${pad(f"$relative%.2f", 8)} $group-$version"
            )
        }
    }
  }

}
