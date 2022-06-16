// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
package com.daml.ledger.sandbox

import com.daml.ledger.runner.common.{CliConfig, CliConfigConverter, Config, PureConfigReaderWriter}
import pureconfig.ConfigConvert
import pureconfig.generic.semiauto.deriveConvert

case class SandboxOnXConfig(
    ledger: Config,
    bridge: BridgeConfig,
)
object SandboxOnXConfig {
  import PureConfigReaderWriter._
  implicit val Convert: ConfigConvert[SandboxOnXConfig] = deriveConvert[SandboxOnXConfig]

  def apply(
      configAdaptor: BridgeConfigAdaptor,
      originalConfig: CliConfig[BridgeConfig],
  ): SandboxOnXConfig = {
    val maxDeduplicationDuration = originalConfig.maxDeduplicationDuration.getOrElse(
      BridgeConfig.DefaultMaximumDeduplicationDuration
    )
    SandboxOnXConfig(
      ledger = CliConfigConverter.toConfig(configAdaptor, originalConfig),
      bridge = originalConfig.extra.copy(maxDeduplicationDuration = maxDeduplicationDuration),
    )
  }
}
