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
import io.circe.JsonObject
import io.circe.syntax.*

/** A schema registered as a version of a subject in a schema registry. */
sealed abstract class RegisteredSchema {

  /** The subject under which the schema is registered. */
  def subject: SubjectName

  /** The identifier of the schema. */
  def id: SchemaId

  /** The version of the subject. */
  def version: SubjectVersion

  /** The schema which is registered. */
  def schema: Schema

  /** The GUID of the schema, if the schema registry supports schema GUIDs. */
  def guid: Option[SchemaGuid]

  /** Returns a new [[RegisteredSchema]] with the specified GUID. */
  def withGuid(guid: SchemaGuid): RegisteredSchema

  /** Returns a new [[RegisteredSchema]] without a GUID. */
  def withoutGuid: RegisteredSchema
}

object RegisteredSchema {
  private final case class RegisteredSchemaImpl(
    override val subject: SubjectName,
    override val id: SchemaId,
    override val version: SubjectVersion,
    override val schema: Schema,
    override val guid: Option[SchemaGuid]
  ) extends RegisteredSchema {
    override def withGuid(guid: SchemaGuid): RegisteredSchema =
      copy(guid = Some(guid))

    override def withoutGuid: RegisteredSchema =
      copy(guid = None)

    override def toString: String =
      s"RegisteredSchema(subject = $subject, id = $id, version = $version, schema = $schema, guid = $guid)"
  }

  /** Returns a new [[RegisteredSchema]] using the specified registration details. */
  def apply(subject: SubjectName, id: SchemaId, version: SubjectVersion, schema: Schema): RegisteredSchema =
    RegisteredSchemaImpl(subject, id, version, schema, None)

  implicit val registeredSchemaDecoder: Decoder[RegisteredSchema] =
    Decoder.instance { c =>
      for {
        subject <- c.get[SubjectName]("subject")
        id <- c.get[SchemaId]("id")
        version <- c.get[SubjectVersion]("version")
        schema <- c.as[Schema]
        guid <- c.get[Option[SchemaGuid]]("guid")
      } yield {
        val registered = RegisteredSchema(subject, id, version, schema)
        guid.fold(registered)(registered.withGuid)
      }
    }

  implicit val registeredSchemaEncoder: Encoder.AsObject[RegisteredSchema] =
    Encoder.AsObject.instance { registered =>
      JsonObject.fromIterable(
        List(
          "subject" -> registered.subject.asJson,
          "version" -> registered.version.asJson,
          "id" -> registered.id.asJson
        ) ++ registered.schema.asJsonObject.toIterable
          ++ registered.guid.map(guid => "guid" -> guid.asJson)
      )
    }

  implicit val registeredSchemaHash: Hash[RegisteredSchema] =
    Hash.fromUniversalHashCode

  implicit val registeredSchemaShow: Show[RegisteredSchema] =
    Show.fromToString
}
