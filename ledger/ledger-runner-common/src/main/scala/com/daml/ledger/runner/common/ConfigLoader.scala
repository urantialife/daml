// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.runner.common

import pureconfig.{ConfigReader, ConfigSource, Derivation}
import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}

import java.io.File

trait ConfigLoader {

  def loadConfig[T](config: TypesafeConfig)(implicit
      reader: Derivation[ConfigReader[T]]
  ): Either[String, T] =
    ConfigSource.fromConfig(config).load[T].left.map { failures =>
      s"Failed to load configuration: \n${failures.prettyPrint()}"
    }

  private def mergeConfigs(
      firstConfig: TypesafeConfig,
      otherConfigs: Seq[TypesafeConfig],
  ): TypesafeConfig =
    otherConfigs.foldLeft(firstConfig)((combined, config) => config.withFallback(combined))

  def toTypesafeConfig(
      configFiles: Seq[File] = Seq(),
      configMap: Map[String, String] = Map(),
  ): TypesafeConfig = {
    val fileConfigs = configFiles.map(ConfigFactory.parseFile)

    val mergedUserConfigs = fileConfigs match {
      case Nil => ConfigFactory.empty()
      case head :: tail =>
        mergeConfigs(head, tail)
    }

    ConfigFactory.invalidateCaches()
    val mergedConfig = mergedUserConfigs.withFallback(ConfigFactory.load())

    val configFromMap = {
      import scala.jdk.CollectionConverters._
      ConfigFactory.parseMap(configMap.asJava)
    }

    mergeConfigs(mergedConfig, Seq(configFromMap))
  }

}

object ConfigLoader extends ConfigLoader {}
