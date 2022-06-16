// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.runner.common

import com.daml.ledger.runner.common.ConfigLoaderSpec.TestScope
import com.typesafe.config.{Config => TypesafeConfig}
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigLoaderSpec extends AnyFlatSpec with Matchers {
  behavior of "ConfigLoader"

  it should "load defaults if no file or key-value map is provided" in new TestScope {
    ConfigLoader.toTypesafeConfig(fallback = empty) shouldBe ConfigFactory.empty()
  }

  it should "override value from the configMap" in new TestScope {
    ConfigLoader.toTypesafeConfig(
      configMap = Map("a" -> "c"),
      fallback = updatedConfig(empty, "a", "b"),
    ) shouldBe updatedConfig(empty, "a", "c")
  }
}

object ConfigLoaderSpec {
  trait TestScope {
    val empty: TypesafeConfig = ConfigFactory.empty()

    def updatedConfig(config: TypesafeConfig, path: String, value: String) =
      config.withValue(path, ConfigValueFactory.fromAnyRef(value))
  }
}
