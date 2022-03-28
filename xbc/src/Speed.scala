// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package xbc

object Speed extends App {

  sealed trait Conf
  object Conf {
    final case class FixedN(group: String, version: String, n: Long) extends Conf
    final case class IncreasingN(group: String, version: String) extends Conf
    final case class WholeGroup(group: String) extends Conf
    final case class PlayGen() extends Conf
  }

  def conf: Conf = {
    //val defaultGroup: String = "nfib"
    def xv(v: String) = v match {
      case "n" => "native"
      case "i" => "interpreter"
      case "ib" => "interpreter-boxed"
      case "ik" => "interpreter-cps"
      case s => s
    }
    args.toList match {
      case Nil =>
        //Conf.WholeGroup(defaultGroup)
        Conf.PlayGen()
      case "play" :: Nil =>
        Conf.PlayGen()
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

  val nfib_n: FUT = Native.nfibRecursive(_)

  val nfib_i: FUT = (x: Long) => {
    def prog = Lang.Examples.nfibProgram(x)
    Interpret.standard(prog)
  }

  val nfib_ib: FUT = (x: Long) => {
    def prog = Lang.Examples.nfibProgram(x)
    val box = InterpretB.run(prog)
    BoxedValue.getNumber(box) match {
      case None => sys.error(s"\n**nfib_ib, expected number, got: box")
      case Some(x) => x
    }
  }

  val nfib_ik: FUT = (x: Long) => {
    def prog = Lang.Examples.nfibProgram(x)
    InterpretK.cps(prog)
  }

  val trip_n: FUT = (x: Long) => {
    val y = lpower(2, x) // make it scalable
    Native.trip(y) / 3L
  }

  val trip_i: FUT = (x: Long) => {
    val y = lpower(2, x)
    def prog = Lang.Examples.tripProgram(y)
    Interpret.standard(prog) / 3L
  }

  val trip_ik: FUT = (x: Long) => {
    val y = lpower(2, x)
    def prog = Lang.Examples.tripProgram(y)
    InterpretK.cps(prog) / 3L
  }

  // map of groups of FUTs
  def m: Map[String, (Option[Double], List[(String, FUT)])] = Map(
    "nfib" -> (None, List( //baseline for speedy on same example
      "interpreter-cps" -> nfib_ik,
      "interpreter-boxed" -> nfib_ib,
      "interpreter" -> nfib_i,
      "native" -> nfib_n,
    )),
    "trip" -> (None, List(
      "interpreter-cps" -> trip_ik,
      "interpreter" -> trip_i,
      "native" -> trip_n,
    )),
  )

  def getVersions(group: String): (Option[Double], List[String]) = {
    m.get(group) match {
      case None => sys.error(s"\n**unknown group: $group")
      case Some((optBaseline, g)) => (optBaseline, (g.map(_._1)))
    }
  }

  def getFUT(group: String, version: String): FUT = {
    m.get(group) match {
      case None => sys.error(s"\n**unknown group: $group")
      case Some((_, g)) =>
        g.toMap.get(version) match {
          case None => sys.error(s"\n**unknown version: $version")
          case Some(f) => f
        }
    }
  }

  conf match {

    case Conf.PlayGen() =>
      Play.gen()

    case Conf.FixedN(group, version, n) =>
      printHeader()
      val setup = Setup(group, version, n)
      while (true) {
        val outcome = runTest(setup)
        printOutcomeLine(outcome, 1.0)
      }

    case Conf.IncreasingN(group, version) =>
      printHeader()
      var n: Long = 15L
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

      val (optBaseline, versions) = getVersions(group)
      versions match {
        case Nil =>
          sys.error(s"\n**no versions for group: $group")
        case leadVersion :: versions =>
          val leadOutcome = runVersion(leadVersion)
          val baselineSpeed = optBaseline match {
            case None => leadOutcome.speed
            case Some(baseline) => baseline
          }
          val leadRelative = leadOutcome.speed / baselineSpeed
          printOutcomeLine(leadOutcome, leadRelative)

          versions.foreach { version =>
            val outcome = runVersion(version)
            val relative = outcome.speed / baselineSpeed
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
    println(s"N   #ops         secs   ops/us    speedup   group:version")
    println(s"----------------------------------------------------------")
  }

  def printOutcomeLine(outcome: Outcome, relative0: Double) = {

    def pad(s: String, max: Int): String = {
      val ss = s.size
      val n = if (ss < max) max - ss else 0
      val padding: String = List.fill(n)(' ').mkString
      s + padding
    }

    outcome match {
      case Outcome(setup, res0, dur0, speed0) =>
        setup match {
          case Setup(group, version, n0) =>
            val n = pad(s"$n0", 3)
            val res = pad(s"$res0", 12)
            val dur = pad(f"$dur0%.2f", 6)
            val speed = pad(f"$speed0%.2f", 9)
            val relative = pad(f"$relative0%.2f", 9)
            val gv = s"$group:$version"
            println(s"$n $res $dur $speed $relative $gv")
        }
    }
  }

  def lpower(base: Long, exponent: Long): Long = {
    if (exponent < 0) {
      sys.error(s"\n**lpower, negative exponent: $exponent")
    } else {
      def powerLoop(i: Long): Long = if (i == 0) 1 else base * powerLoop(i - 1)
      powerLoop(exponent)
    }
  }

}
