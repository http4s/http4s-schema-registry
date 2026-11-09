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

/** The error code of an error response from a schema registry. */
sealed abstract class SchemaRegistryErrorCode {

  /** The error code as a number. */
  def value: Int
}

object SchemaRegistryErrorCode {
  private final case class SchemaRegistryErrorCodeImpl(override val value: Int)
    extends SchemaRegistryErrorCode {
    override def toString: String =
      s"SchemaRegistryErrorCode($value)"
  }

  /** Returns a [[SchemaRegistryErrorCode]] with the specified value. */
  def apply(value: Int): SchemaRegistryErrorCode =
    SchemaRegistryErrorCodeImpl(value)

  /** Error code for when a subject does not exist. */
  val SubjectNotFound: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode(40401)

  /** Error code for when a version of a subject does not exist. */
  val VersionNotFound: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode(40402)

  /** Error code for when a schema does not exist. */
  val SchemaNotFound: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode(40403)

  /** Error code for when a schema is incompatible with earlier subject versions. */
  val IncompatibleSchema: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode(40901)

  /** Error code for when a schema is invalid. */
  val InvalidSchema: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode(42201)

  /** Error code for when a version is invalid. */
  val InvalidVersion: SchemaRegistryErrorCode =
    SchemaRegistryErrorCode(42202)

  implicit val schemaRegistryErrorCodeDecoder: Decoder[SchemaRegistryErrorCode] =
    Decoder[Int].map(apply)

  implicit val schemaRegistryErrorCodeEncoder: Encoder[SchemaRegistryErrorCode] =
    Encoder[Int].contramap(_.value)

  implicit val schemaRegistryErrorCodeHash: Hash[SchemaRegistryErrorCode] =
    Hash.fromUniversalHashCode

  implicit val schemaRegistryErrorCodeShow: Show[SchemaRegistryErrorCode] =
    Show.fromToString
}
