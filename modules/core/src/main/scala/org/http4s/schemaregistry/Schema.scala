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

/** A schema as it is stored in a schema registry. */
sealed abstract class Schema {

  /** The definition of the schema, for example the Avro schema. */
  def definition: String

  /** The type of the schema. */
  def schemaType: SchemaType

  /** References to schemas registered under other subjects. */
  def references: List[SchemaReference]

  /** Returns a new [[Schema]] with the specified type. */
  def withSchemaType(schemaType: SchemaType): Schema

  /** Returns a new [[Schema]] with the specified references. */
  def withReferences(references: List[SchemaReference]): Schema
}

object Schema {
  private final case class SchemaImpl(
    override val definition: String,
    override val schemaType: SchemaType,
    override val references: List[SchemaReference]
  ) extends Schema {
    override def withSchemaType(schemaType: SchemaType): Schema =
      copy(schemaType = schemaType)

    override def withReferences(references: List[SchemaReference]): Schema =
      copy(references = references)

    override def toString: String =
      s"Schema(definition = $definition, schemaType = $schemaType, references = $references)"
  }

  /** Returns an Avro schema with the specified definition and without references. */
  def apply(definition: String): Schema =
    SchemaImpl(definition, SchemaType.Avro, Nil)

  implicit val schemaDecoder: Decoder[Schema] =
    Decoder.instance { c =>
      for {
        definition <- c.get[String]("schema")
        schemaType <- c.getOrElse[SchemaType]("schemaType")(SchemaType.Avro)
        references <- c.getOrElse[List[SchemaReference]]("references")(Nil)
      } yield Schema(definition).withSchemaType(schemaType).withReferences(references)
    }

  implicit val schemaEncoder: Encoder.AsObject[Schema] =
    Encoder.AsObject.instance { schema =>
      JsonObject.fromIterable(
        List(
          List("schema" -> schema.definition.asJson),
          if (schema.schemaType == SchemaType.Avro) Nil else List("schemaType" -> schema.schemaType.asJson),
          if (schema.references.isEmpty) Nil else List("references" -> schema.references.asJson)
        ).flatten
      )
    }

  implicit val schemaHash: Hash[Schema] =
    Hash.fromUniversalHashCode

  implicit val schemaShow: Show[Schema] =
    Show.fromToString
}
