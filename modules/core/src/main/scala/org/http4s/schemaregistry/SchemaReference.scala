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

/** Reference from a schema to a schema registered under another subject. */
sealed abstract class SchemaReference {

  /** The name being used to reference the schema at the subject version. */
  def name: String

  /** The subject under which the referenced schema is registered. */
  def subject: SubjectName

  /** The version of the subject which is referenced. */
  def version: SubjectVersion
}

object SchemaReference {
  private final case class SchemaReferenceImpl(
    override val name: String,
    override val subject: SubjectName,
    override val version: SubjectVersion
  ) extends SchemaReference {
    override def toString: String =
      s"SchemaReference(name = $name, subject = $subject, version = $version)"
  }

  /** Returns a [[SchemaReference]] for the specified name to the specified subject version. */
  def apply(name: String, subject: SubjectName, version: SubjectVersion): SchemaReference =
    SchemaReferenceImpl(name, subject, version)

  implicit val schemaReferenceDecoder: Decoder[SchemaReference] =
    Decoder.forProduct3("name", "subject", "version")(apply)

  implicit val schemaReferenceEncoder: Encoder.AsObject[SchemaReference] =
    Encoder.forProduct3("name", "subject", "version") { (reference: SchemaReference) =>
      (reference.name, reference.subject, reference.version)
    }

  implicit val schemaReferenceHash: Hash[SchemaReference] =
    Hash.fromUniversalHashCode

  implicit val schemaReferenceShow: Show[SchemaReference] =
    Show.fromToString
}
