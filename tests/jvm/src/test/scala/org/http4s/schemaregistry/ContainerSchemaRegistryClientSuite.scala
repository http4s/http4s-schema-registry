/*
 * Copyright 2026 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.schemaregistry

import cats.effect.IO
import cats.effect.Resource
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder

abstract class ContainerSchemaRegistryClientSuite extends SchemaRegistryClientSuite {

  /** The registry being tested, returning its base URI. */
  def registry: Resource[IO, Uri]

  override def sharedResource: Resource[IO, SchemaRegistryClient[IO]] =
    for {
      uri <- registry
      client <- EmberClientBuilder.default[IO].build
      schemaRegistryClient <- Resource.eval(SchemaRegistryClientBuilder.default(client, uri).build)
    } yield schemaRegistryClient
}

object ConfluentSchemaRegistryClientSuite extends ContainerSchemaRegistryClientSuite {
  override def registry: Resource[IO, Uri] =
    Containers.confluent

  override def supportsGuids: Boolean =
    true

  override def incompatibleSchemaErrorCode: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode.IncompatibleSchema

  override def compatibleWithMissingSubjects: Boolean =
    true
}

object KarapaceSchemaRegistryClientSuite extends ContainerSchemaRegistryClientSuite {
  override def registry: Resource[IO, Uri] =
    Containers.karapace

  override def supportsGuids: Boolean =
    false

  override def incompatibleSchemaErrorCode: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode(409)

  override def compatibleWithMissingSubjects: Boolean =
    false

  test("getSchemaByGuid raises an error, since Karapace does not support schema GUIDs") { client =>
    client.getSchemaByGuid(SchemaGuid("00000000-0000-0000-0000-000000000001")).attempt.map { result =>
      matches(result) { case Left(_: SchemaRegistryError) => success }
    }
  }
}
