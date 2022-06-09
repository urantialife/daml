// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.runner.common

import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}

import java.io.File

class HoconCli(programName: String) {

  val argParser = new scopt.OptionParser[HoconCli.Cli](programName) {
    opt[Map[String, String]]('C', "config key-value's")
      .text(
        "Set configuration key value pairs directly. Can be useful for providing simple short config info."
      )
      .valueName("<key1>=<value1>,<key2>=<value2>")
      .unbounded()
      .action { (map, cli) =>
        cli.copy(configMap = map ++ cli.configMap)
      }
    opt[Seq[File]]('c', "config")
      .text(
        "Set configuration file(s). If several configuration files assign values to the same key, the last value is taken."
      )
      .valueName("<file1>,<file2>,...")
      .unbounded()
      .action((files, cli) => cli.copy(configFiles = cli.configFiles ++ files))
  }

  def parse(args: Array[String]): Option[HoconCli.Cli] = {
    argParser.parse(args, HoconCli.Cli())
  }

}

object HoconCli {
  case class Cli(
      configFiles: Seq[File] = Seq(),
      configMap: Map[String, String] = Map(),
  )
  def mergeConfigs(firstConfig: TypesafeConfig, otherConfigs: Seq[TypesafeConfig]): TypesafeConfig =
    otherConfigs.foldLeft(firstConfig)((combined, config) => config.withFallback(combined))

  def loadConfigWithOverrides(programName: String, args: Array[String]): TypesafeConfig = {
    val cliOptions = new HoconCli(programName).parse(args).getOrElse(sys.exit(1))

    val fileConfigs = cliOptions.configFiles.map(ConfigFactory.parseFile)

    val mergedUserConfigs = fileConfigs match {
      case Nil => ConfigFactory.empty()
      case head :: tail =>
        mergeConfigs(head, tail)
    }

    ConfigFactory.invalidateCaches()
    val mergedConfig = mergedUserConfigs.withFallback(ConfigFactory.load())

    val configFromMap = {
      import scala.jdk.CollectionConverters._
      ConfigFactory.parseMap(cliOptions.configMap.asJava)
    }

    mergeConfigs(mergedConfig, Seq(configFromMap))
  }
}
