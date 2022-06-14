// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.runner.common

import pureconfig.ConfigReader.Result
import pureconfig.{ConfigReader, ConfigSource, Derivation}

trait ConfigLoader {

  def loadConfig[T](config: com.typesafe.config.Config)(implicit
      reader: Derivation[ConfigReader[T]]
  ): Result[T] =
    ConfigSource.fromConfig(config).load[T]

  def loadConfigUnsafe[T](config: com.typesafe.config.Config)(implicit
      reader: Derivation[ConfigReader[T]]
  ): T = {
    loadConfig(config) match {
      case Left(value) => sys.error(value.toList.map(_.description).mkString(","))
      case Right(value) => value
    }
  }
}

object ConfigLoader extends ConfigLoader {}
