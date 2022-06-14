// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.sandbox

import com.daml.ledger.resources.{Resource, ResourceContext, ResourceOwner}
import com.daml.ledger.runner.common._
import com.daml.logging.ContextualizedLogger
import scopt.OParser

import java.time.Duration

object CliSandboxOnXRunner {
  private val logger = ContextualizedLogger.get(getClass)

  val RunnerName = "sandbox-on-x"

  val bridgeConfigParser: OParser[_, CliConfig[BridgeConfig]] = {
    val builder = OParser.builder[CliConfig[BridgeConfig]]
    import builder._
    OParser.sequence(
      opt[Int]("bridge-submission-buffer-size")
        .text("Submission buffer size. Defaults to 500.")
        .action((p, c) => c.copy(extra = c.extra.copy(submissionBufferSize = p))),
      opt[Unit]("disable-conflict-checking")
        .hidden()
        .text("Disable ledger-side submission conflict checking.")
        .action((_, c) => c.copy(extra = c.extra.copy(conflictCheckingEnabled = false))),
      opt[Boolean](name = "implicit-party-allocation")
        .optional()
        .action((x, c) => c.copy(implicitPartyAllocation = x))
        .text(
          s"When referring to a party that doesn't yet exist on the ledger, the participant will implicitly allocate that party."
            + s" You can optionally disable this behavior to bring participant into line with other ledgers."
        ),
      checkConfig(c =>
        Either.cond(
          c.maxDeduplicationDuration.forall(_.compareTo(Duration.ofHours(1L)) <= 0),
          (),
          "Maximum supported deduplication duration is one hour",
        )
      ),
    )
  }

  def owner(
      args: collection.Seq[String],
      manipulateConfig: CliConfig[BridgeConfig] => CliConfig[BridgeConfig] = identity,
  ): ResourceOwner[Unit] =
    CliConfig
      .owner(
        RunnerName,
        bridgeConfigParser,
        BridgeConfig.Default,
        args,
      )
      .map(manipulateConfig)
      .flatMap(owner)

  private def run(
      originalConfig: CliConfig[BridgeConfig]
  )(implicit context: ResourceContext): Resource[Unit] = {
    val configAdaptor: BridgeConfigAdaptor = new BridgeConfigAdaptor
    val typeSafeConfig = SandboxOnXRunner.toTypesafeConfig(
      originalConfig.configFiles,
      originalConfig.configMap,
    )
    val sandboxOnXConfig: SandboxOnXConfig =
      ConfigLoader.loadConfigUnsafe[SandboxOnXConfig](typeSafeConfig)
    logger.withoutContext.info(s"Config used: ${ConfigRenderer.render(sandboxOnXConfig)}")
    SandboxOnXRunner
      .run(configAdaptor, sandboxOnXConfig.ledger, sandboxOnXConfig.bridge)
      .map(_ => ())
  }

  private def toSandboxOnXConfig(
      configAdaptor: BridgeConfigAdaptor,
      originalConfig: CliConfig[BridgeConfig],
  ): SandboxOnXConfig =
    SandboxOnXConfig(
      ledger = CliConfigConverter.toConfig(configAdaptor, originalConfig),
      bridge = originalConfig.extra.copy(maxDeduplicationDuration =
        originalConfig.maxDeduplicationDuration.getOrElse(
          BridgeConfig.DefaultMaximumDeduplicationDuration
        )
      ),
    )

  private def runLegacy(
      originalConfig: CliConfig[BridgeConfig]
  )(implicit context: ResourceContext): Resource[Unit] = {
    val configAdaptor: BridgeConfigAdaptor = new BridgeConfigAdaptor
    val sandboxOnXConfig: SandboxOnXConfig = toSandboxOnXConfig(configAdaptor, originalConfig)
    logger.withoutContext.info(s"Config used: ${ConfigRenderer.render(sandboxOnXConfig)}")
    SandboxOnXRunner
      .run(
        configAdaptor,
        sandboxOnXConfig.ledger,
        sandboxOnXConfig.bridge,
      )
      .map(_ => ())
  }

  private def owner(originalConfig: CliConfig[BridgeConfig]): ResourceOwner[Unit] =
    new ResourceOwner[Unit] {
      override def acquire()(implicit context: ResourceContext): Resource[Unit] = {
        originalConfig.mode match {
          case Mode.Run =>
            run(originalConfig)
          case Mode.DumpIndexMetadata(jdbcUrls) =>
            DumpIndexMetadata(jdbcUrls)
            sys.exit(0)
          case Mode.ConvertConfig =>
            val configAdaptor: BridgeConfigAdaptor = new BridgeConfigAdaptor
            Console.out.println(
              ConfigRenderer.render(toSandboxOnXConfig(configAdaptor, originalConfig))
            )
            sys.exit(0)
          case Mode.RunLegacy =>
            runLegacy(originalConfig)
        }
      }
    }
}
