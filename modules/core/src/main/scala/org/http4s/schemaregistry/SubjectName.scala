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

/** The name of a subject in a schema registry. */
sealed abstract class SubjectName {

  /** The name of the subject. */
  def value: String
}

object SubjectName {
  private final case class SubjectNameImpl(override val value: String) extends SubjectName {
    override def toString: String =
      s"SubjectName($value)"
  }

  /** Returns a [[SubjectName]] with the specified value. */
  def apply(value: String): SubjectName =
    SubjectNameImpl(value)

  implicit val subjectNameDecoder: Decoder[SubjectName] =
    Decoder[String].map(apply)

  implicit val subjectNameEncoder: Encoder[SubjectName] =
    Encoder[String].contramap(_.value)

  implicit val subjectNameHash: Hash[SubjectName] =
    Hash.fromUniversalHashCode

  implicit val subjectNameShow: Show[SubjectName] =
    Show.fromToString
}
