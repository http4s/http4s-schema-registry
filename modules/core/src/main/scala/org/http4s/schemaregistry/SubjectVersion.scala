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

/** Version of a schema registered under a subject, starting from `1`. */
sealed abstract class SubjectVersion {

  /** The version as a number. */
  def value: Int
}

object SubjectVersion {
  private final case class SubjectVersionImpl(override val value: Int) extends SubjectVersion {
    override def toString: String =
      s"SubjectVersion($value)"
  }

  /** Returns a [[SubjectVersion]] with the specified value. */
  def apply(value: Int): SubjectVersion =
    SubjectVersionImpl(value)

  implicit val subjectVersionDecoder: Decoder[SubjectVersion] =
    Decoder[Int].map(apply)

  implicit val subjectVersionEncoder: Encoder[SubjectVersion] =
    Encoder[Int].contramap(_.value)

  implicit val subjectVersionOrder: Order[SubjectVersion] with Hash[SubjectVersion] =
    new Order[SubjectVersion] with Hash[SubjectVersion] {
      override def compare(x: SubjectVersion, y: SubjectVersion): Int =
        Integer.compare(x.value, y.value)

      override def hash(x: SubjectVersion): Int =
        x.hashCode
    }

  implicit val subjectVersionShow: Show[SubjectVersion] =
    Show.fromToString
}
