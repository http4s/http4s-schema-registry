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
import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder

/** The identifier for a schema in a schema registry. */
sealed abstract class SchemaId {

  /** The identifier as a number. */
  def value: Int
}

object SchemaId {
  private final case class SchemaIdImpl(override val value: Int) extends SchemaId {
    override def toString: String =
      s"SchemaId($value)"
  }

  /** Returns a [[SchemaId]] with the specified value. */
  def apply(value: Int): SchemaId =
    SchemaIdImpl(value)

  implicit val schemaIdDecoder: Decoder[SchemaId] =
    Decoder[Int].map(apply)

  implicit val schemaIdEncoder: Encoder[SchemaId] =
    Encoder[Int].contramap(_.value)

  implicit val schemaIdOrder: Order[SchemaId] with Hash[SchemaId] =
    new Order[SchemaId] with Hash[SchemaId] {
      override def compare(x: SchemaId, y: SchemaId): Int =
        Integer.compare(x.value, y.value)

      override def hash(x: SchemaId): Int =
        x.hashCode
    }

  implicit val schemaIdShow: Show[SchemaId] =
    Show.fromToString
}
