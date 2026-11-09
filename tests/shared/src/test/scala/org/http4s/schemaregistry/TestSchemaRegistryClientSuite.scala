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

object TestSchemaRegistryClientSuite extends SchemaRegistryClientSuite {
  override def sharedResource: Resource[IO, SchemaRegistryClient[IO]] =
    Resource.eval {
      TestSchemaRegistry
        .withCompatibility[IO] { (_, schema, _) =>
          if (schema == SchemaRegistryClientSuite.incompatible)
            Compatibility.Incompatible(List("Incompatible type for field name"))
          else Compatibility.Compatible
        }
        .flatMap(_.client.build)
    }

  override def supportsGuids: Boolean =
    true

  override def incompatibleSchemaErrorCode: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode.IncompatibleSchema

  override def compatibleWithMissingSubjects: Boolean =
    true
}
