// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.sandbox

import com.daml.ledger.resources.{Resource, ResourceContext, ResourceOwner}
import com.daml.ledger.runner.common._
import com.daml.logging.ContextualizedLogger
import com.daml.resources.ProgramResource

object CliSandboxOnXRunner {
  private val logger = ContextualizedLogger.get(getClass)

  val RunnerName = "sandbox-on-x"

  def program[T](acquireResource: ResourceContext => Resource[T]): Unit = {
    new ProgramResource(ResourceOwner.forResource(acquireResource))
      .run(ResourceContext.apply)
  }

  def run(
      args: collection.Seq[String],
      manipulateConfig: CliConfig[BridgeConfig] => CliConfig[BridgeConfig] = identity,
  ): Unit =
    CliConfig
      .parse(RunnerName, BridgeConfig.Parser, BridgeConfig.Default, args)
      .map(manipulateConfig)
      .foreach(runProgram)

  private def runProgram(config: CliConfig[BridgeConfig]): Unit =
    config.mode match {
      case Mode.Run =>
        val configAdaptor: BridgeConfigAdaptor = new BridgeConfigAdaptor
        val sandboxOnXConfig: Either[String, SandboxOnXConfig] =
          ConfigLoader.loadConfig[SandboxOnXConfig](
            ConfigLoader.toTypesafeConfig(
              config.configFiles,
              config.configMap,
            )
          )
        sandboxOnXConfig.left.foreach(System.err.println)

        sandboxOnXConfig.foreach { sandboxOnXConfig =>
          program(implicit context => runSandboxOnX(configAdaptor, sandboxOnXConfig))
        }
      case Mode.DumpIndexMetadata(jdbcUrls) =>
        program(implicit context => DumpIndexMetadata(jdbcUrls))
      case Mode.ConvertConfig =>
        Console.out.println(
          ConfigRenderer.render(SandboxOnXConfig(new BridgeConfigAdaptor, config))
        )
      case Mode.RunLegacy =>
        val configAdaptor: BridgeConfigAdaptor = new BridgeConfigAdaptor
        val sandboxOnXConfig: SandboxOnXConfig = SandboxOnXConfig(configAdaptor, config)
        program(implicit context => runSandboxOnX(configAdaptor, sandboxOnXConfig))
    }

  private def runSandboxOnX(
      configAdaptor: BridgeConfigAdaptor,
      sandboxOnXConfig: SandboxOnXConfig,
  )(implicit context: ResourceContext): Resource[Unit] = {
    logger.withoutContext.info(s"Config used: ${ConfigRenderer.render(sandboxOnXConfig)}")
    SandboxOnXRunner
      .run(
        configAdaptor,
        sandboxOnXConfig.ledger,
        sandboxOnXConfig.bridge,
      )
      .map(_ => ())
  }

}
