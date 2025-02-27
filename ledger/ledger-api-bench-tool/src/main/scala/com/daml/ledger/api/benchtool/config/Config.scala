// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.api.benchtool.config

import com.daml.ledger.api.tls.TlsConfiguration
import com.daml.metrics.MetricsReporter

import java.io.File
import scala.concurrent.duration._

case class Config(
    ledger: Config.Ledger,
    concurrency: Config.Concurrency,
    tls: TlsConfiguration,
    workflow: WorkflowConfig,
    reportingPeriod: FiniteDuration,
    workflowConfigFile: Option[File],
    maxInFlightCommands: Int,
    submissionBatchSize: Int,
    metricsReporter: MetricsReporter,
    authorizationTokenSecret: Option[String],
    latencyTest: Boolean,
    maxLatencyObjectiveMillis: Long,
)

object Config {
  case class Ledger(
      hostname: String,
      port: Int,
  )

  case class Concurrency(
      corePoolSize: Int,
      maxPoolSize: Int,
      keepAliveTime: Long,
      maxQueueLength: Int,
  )

  val Default: Config =
    Config(
      ledger = Config.Ledger(
        hostname = "localhost",
        port = 6865,
      ),
      concurrency = Config.Concurrency(
        corePoolSize = 2,
        maxPoolSize = 8,
        keepAliveTime = 30,
        maxQueueLength = 10000,
      ),
      tls = TlsConfiguration.Empty.copy(enabled = false),
      workflow = WorkflowConfig(),
      reportingPeriod = 5.seconds,
      workflowConfigFile = None,
      maxInFlightCommands = 100,
      submissionBatchSize = 100,
      metricsReporter = MetricsReporter.Console,
      authorizationTokenSecret = None,
      latencyTest = false,
      maxLatencyObjectiveMillis = 1000L,
    )
}
