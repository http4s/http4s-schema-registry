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

import io.circe.Json
import org.http4s.Status
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object Generators {
  private val specialCharacters: Gen[String] =
    Gen.oneOf(
      List("\"", "\\", "/", "%", "%2F", "+", " ", "?", "#", "&", "=", ":", ";", "@", ".", "..", "\n", "\t") ++
        List("\u0000", "é", "€", "😀")
    )

  private val characters: Gen[String] =
    Gen.frequency(
      4 -> Gen.alphaNumChar.map(_.toString),
      4 -> Arbitrary.arbitrary[Char].map(_.toString),
      1 -> specialCharacters
    )

  val stringsWithSpecialCharacters: Gen[String] =
    Gen.listOf(characters).map(_.mkString)

  private val nonEmptyStringsWithSpecialCharacters: Gen[String] =
    Gen.zip(characters, stringsWithSpecialCharacters).map { case (first, rest) => first + rest }

  def listsOfUpToThree[A](values: Gen[A]): Gen[List[A]] =
    Gen.choose(0, 3).flatMap(Gen.listOfN(_, values))

  val jsonValues: Gen[Json] =
    jsonValuesNestedUpTo(depth = 2)

  private def jsonValuesNestedUpTo(depth: Int): Gen[Json] = {
    val primitives =
      Gen.oneOf(
        Gen.const(Json.Null),
        Arbitrary.arbitrary[Boolean].map(Json.fromBoolean),
        Arbitrary.arbitrary[Long].map(Json.fromLong),
        Arbitrary.arbitrary[Double].map(Json.fromDoubleOrNull),
        stringsWithSpecialCharacters.map(Json.fromString)
      )

    if (depth <= 0) primitives
    else {
      val nested = jsonValuesNestedUpTo(depth - 1)
      Gen.frequency(
        2 -> primitives,
        1 -> listsOfUpToThree(nested).map(Json.fromValues),
        1 -> listsOfUpToThree(Gen.zip(stringsWithSpecialCharacters, nested)).map(Json.fromFields)
      )
    }
  }

  val schemaIds: Gen[SchemaId] =
    Arbitrary.arbitrary[Int].map(SchemaId(_))

  val schemaGuids: Gen[SchemaGuid] =
    Gen.frequency(3 -> Gen.uuid.map(_.toString), 1 -> nonEmptyStringsWithSpecialCharacters).map(SchemaGuid(_))

  val subjectNames: Gen[SubjectName] =
    nonEmptyStringsWithSpecialCharacters.map(SubjectName(_))

  val subjectVersions: Gen[SubjectVersion] =
    Arbitrary.arbitrary[Int].map(SubjectVersion(_))

  val schemaTypes: Gen[SchemaType] =
    Gen.frequency(
      2 -> Gen.const(SchemaType.Avro),
      1 -> Gen.oneOf(SchemaType.Json, SchemaType.Protobuf),
      1 -> stringsWithSpecialCharacters.map(SchemaType(_))
    )

  val schemaReferences: Gen[SchemaReference] =
    for {
      name <- stringsWithSpecialCharacters
      subject <- subjectNames
      version <- subjectVersions
    } yield SchemaReference(name, subject, version)

  val schemas: Gen[Schema] =
    for {
      definition <- Gen.oneOf(stringsWithSpecialCharacters, jsonValues.map(_.noSpaces))
      schemaType <- schemaTypes
      references <- listsOfUpToThree(schemaReferences)
    } yield Schema(definition).withSchemaType(schemaType).withReferences(references)

  val registeredSchemas: Gen[RegisteredSchema] =
    for {
      subject <- subjectNames
      id <- schemaIds
      version <- subjectVersions
      schema <- schemas
      guid <- Gen.option(schemaGuids)
    } yield {
      val registered = RegisteredSchema(subject, id, version, schema)
      guid.fold(registered)(registered.withGuid)
    }

  val compatibilities: Gen[Compatibility] =
    Gen.frequency(
      1 -> Gen.const(Compatibility.Compatible),
      2 -> listsOfUpToThree(stringsWithSpecialCharacters).map(Compatibility.Incompatible)
    )

  val errorCodes: Gen[SchemaRegistryErrorCode] = {
    val namedErrorCodes =
      Gen.oneOf(
        SchemaRegistryErrorCode.SubjectNotFound,
        SchemaRegistryErrorCode.VersionNotFound,
        SchemaRegistryErrorCode.SchemaNotFound,
        SchemaRegistryErrorCode.IncompatibleSchema,
        SchemaRegistryErrorCode.InvalidSchema,
        SchemaRegistryErrorCode.InvalidVersion
      )

    Gen.frequency(1 -> namedErrorCodes, 1 -> Arbitrary.arbitrary[Int].map(SchemaRegistryErrorCode(_)))
  }

  val statuses: Gen[Status] =
    Gen.choose(100, 599).map(statusOf)

  val unsuccessfulStatuses: Gen[Status] = {
    val registryErrorStatuses =
      Gen.oneOf(Status.NotFound, Status.Conflict, Status.UnprocessableContent, Status.InternalServerError)

    Gen.frequency(1 -> registryErrorStatuses, 1 -> Gen.choose(300, 599).map(statusOf))
  }

  private def statusOf(code: Int): Status =
    Status.fromInt(code).fold(throw _, identity)
}
