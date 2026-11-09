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

/** A globally unique identifier for a schema in a schema registry. */
sealed abstract class SchemaGuid {

  /** The identifier as it is returned by the schema registry. */
  def value: String
}

object SchemaGuid {
  private final case class SchemaGuidImpl(override val value: String) extends SchemaGuid {
    override def toString: String =
      s"SchemaGuid($value)"
  }

  /** Returns a [[SchemaGuid]] with the specified value. */
  def apply(value: String): SchemaGuid =
    SchemaGuidImpl(value)

  implicit val schemaGuidDecoder: Decoder[SchemaGuid] =
    Decoder[String].map(apply)

  implicit val schemaGuidEncoder: Encoder[SchemaGuid] =
    Encoder[String].contramap(_.value)

  implicit val schemaGuidHash: Hash[SchemaGuid] =
    Hash.fromUniversalHashCode

  implicit val schemaGuidShow: Show[SchemaGuid] =
    Show.fromToString
}
