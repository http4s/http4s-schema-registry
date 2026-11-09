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

import cats.Hash
import cats.Show
import io.circe.Decoder
import io.circe.Encoder

/** The type of a schema, as identified by a schema registry. */
sealed abstract class SchemaType {

  /** The name of the schema type. */
  def name: String
}

object SchemaType {
  private final case class SchemaTypeImpl(override val name: String) extends SchemaType {
    override def toString: String =
      s"SchemaType($name)"
  }

  /** Returns a [[SchemaType]] with the specified name. */
  def apply(name: String): SchemaType =
    SchemaTypeImpl(name)

  /** Avro schemas. */
  val Avro: SchemaType =
    SchemaType("AVRO")

  /** JSON Schema schemas. */
  val Json: SchemaType =
    SchemaType("JSON")

  /** Protocol Buffers schemas. */
  val Protobuf: SchemaType =
    SchemaType("PROTOBUF")

  implicit val schemaTypeDecoder: Decoder[SchemaType] =
    Decoder[String].map(apply)

  implicit val schemaTypeEncoder: Encoder[SchemaType] =
    Encoder[String].contramap(_.name)

  implicit val schemaTypeHash: Hash[SchemaType] =
    Hash.fromUniversalHashCode

  implicit val schemaTypeShow: Show[SchemaType] =
    Show.fromToString
}
