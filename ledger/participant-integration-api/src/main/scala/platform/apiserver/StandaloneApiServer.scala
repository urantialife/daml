// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.apiserver

import java.time.Clock
import akka.actor.ActorSystem
import akka.stream.Materializer
import com.daml.api.util.TimeProvider
import com.daml.buildinfo.BuildInfo
import com.daml.ledger.api.auth.interceptor.AuthorizationInterceptor
import com.daml.ledger.api.auth.{AuthService, Authorizer}
import com.daml.ledger.api.health.HealthChecks
import com.daml.ledger.configuration.LedgerId
import com.daml.ledger.participant.state.index.v2.{IndexService, UserManagementStore}
import com.daml.ledger.participant.state.{v2 => state}
import com.daml.ledger.resources.ResourceOwner
import com.daml.lf.data.Ref
import com.daml.lf.engine.Engine
import com.daml.logging.{ContextualizedLogger, LoggingContext}
import com.daml.metrics.Metrics
import com.daml.platform.services.time.TimeProviderType
import com.daml.ports.{Port, PortFiles}
import com.daml.telemetry.TelemetryContext
import io.grpc.{BindableService, ServerInterceptor}
import scalaz.{-\/, \/-}

import scala.collection.immutable
import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success, Try}

object StandaloneApiServer {
  private val logger = ContextualizedLogger.get(this.getClass)

  def apply(
      indexService: IndexService,
      userManagementStore: UserManagementStore,
      ledgerId: LedgerId,
      participantId: Ref.ParticipantId,
      config: ApiServerConfig,
      optWriteService: Option[state.WriteService],
      healthChecks: HealthChecks,
      metrics: Metrics,
      timeServiceBackend: Option[TimeServiceBackend] = None,
      otherServices: immutable.Seq[BindableService] = immutable.Seq.empty,
      otherInterceptors: List[ServerInterceptor] = List.empty,
      engine: Engine,
      servicesExecutionContext: ExecutionContextExecutor,
      checkOverloaded: TelemetryContext => Option[state.SubmissionResult] =
        _ => None, // Used for Canton rate-limiting,
      ledgerFeatures: LedgerFeatures,
      authService: AuthService,
  )(implicit
      actorSystem: ActorSystem,
      materializer: Materializer,
      loggingContext: LoggingContext,
  ): ResourceOwner[ApiServer] = {

    def writePortFile(port: Port): Try[Unit] = {
      config.portFile match {
        case Some(path) =>
          PortFiles.write(path, port) match {
            case -\/(err) => Failure(new RuntimeException(err.toString))
            case \/-(()) => Success(())
          }
        case None =>
          Success(())
      }
    }

    val authorizer = new Authorizer(
      Clock.systemUTC.instant _,
      ledgerId,
      participantId,
      userManagementStore,
      servicesExecutionContext,
      userRightsCheckIntervalInSeconds = config.userManagement.cacheExpiryAfterWriteInSeconds,
      akkaScheduler = actorSystem.scheduler,
    )
    val healthChecksWithIndexService = healthChecks + ("index" -> indexService)

    for {
      executionSequencerFactory <- new ExecutionSequencerFactoryOwner()
      apiServicesOwner = new ApiServices.Owner(
        participantId = participantId,
        optWriteService = optWriteService,
        indexService = indexService,
        authorizer = authorizer,
        engine = engine,
        timeProvider = timeServiceBackend.getOrElse(TimeProvider.UTC),
        timeProviderType =
          timeServiceBackend.fold[TimeProviderType](TimeProviderType.WallClock)(_ =>
            TimeProviderType.Static
          ),
        configurationLoadTimeout = config.configurationLoadTimeout,
        initialLedgerConfiguration = config.initialLedgerConfiguration,
        commandConfig = config.command,
        partyConfig = config.party,
        optTimeServiceBackend = timeServiceBackend,
        servicesExecutionContext = servicesExecutionContext,
        metrics = metrics,
        healthChecks = healthChecksWithIndexService,
        seedService = SeedService(config.seeding),
        managementServiceTimeout = config.managementServiceTimeout,
        checkOverloaded = checkOverloaded,
        userManagementStore = userManagementStore,
        ledgerFeatures = ledgerFeatures,
        userManagementConfig = config.userManagement,
        apiStreamShutdownTimeout = config.apiStreamShutdownTimeout,
      )(materializer, executionSequencerFactory, loggingContext)
        .map(_.withServices(otherServices))
      apiServer <- new LedgerApiServer(
        apiServicesOwner,
        config.port,
        config.maxInboundMessageSize,
        config.address,
        config.tls,
        AuthorizationInterceptor(
          authService,
          Option.when(config.userManagement.enabled)(userManagementStore),
          servicesExecutionContext,
        ) :: otherInterceptors,
        servicesExecutionContext,
        metrics,
        config.rateLimitingConfig,
      )
      _ <- ResourceOwner.forTry(() => writePortFile(apiServer.port))
    } yield {
      logger.info(
        s"Initialized API server version ${BuildInfo.Version} with ledger-id = $ledgerId, port = ${apiServer.port}"
      )
      apiServer
    }
  }
}
