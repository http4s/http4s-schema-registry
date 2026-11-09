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

import cats.effect.IO
import io.circe.jawn
import java.util.concurrent.atomic.AtomicLong
import org.http4s.Status
import org.http4s.schemaregistry.SchemaRegistryClientSuite.*
import weaver.Expectations
import weaver.IOSuite
import weaver.SourceLocation

abstract class SchemaRegistryClientSuite extends IOSuite {
  override type Res = SchemaRegistryClient[IO]

  /** Whether the registry supports schema GUIDs. */
  def supportsGuids: Boolean

  /** The error code of the registry for incompatible schemas. */
  def incompatibleSchemaErrorCode: SchemaRegistryErrorCode

  /**
    * Whether the registry considers schemas compatible with the
    * latest version of subjects which do not exist, instead of
    * responding with a not found response.
    */
  def compatibleWithMissingSubjects: Boolean

  /** Returns a subject which has not been used before in this test run. */
  def newSubject: IO[SubjectName] =
    IO(SubjectName(s"test-${subjects.incrementAndGet()}"))

  /** Expects the schemas to be the same, regardless of the formatting of their definitions. */
  def expectSameSchema(expected: Schema, found: Schema)(implicit loc: SourceLocation): Expectations =
    expect.eql(expected.schemaType, found.schemaType) &&
      expect.eql(expected.references, found.references) &&
      expect.same(jawn.parse(expected.definition), jawn.parse(found.definition))

  /** Expects the result to be a [[SchemaRegistryError]] with the status and error code. */
  def expectError(result: Either[Throwable, Any], status: Status, errorCode: SchemaRegistryErrorCode)(implicit
    loc: SourceLocation
  ): Expectations =
    matches(result) { case Left(error: SchemaRegistryError) =>
      expect.same(status, error.status) && expect.eql(Some(errorCode), error.errorCode)
    }

  test("registerSchema returns the same id when registering a schema again") { client =>
    for {
      subject <- newSubject
      id <- client.registerSchema(subject, first)
      again <- client.registerSchema(subject, first)
    } yield expect.eql(id, again)
  }

  test("registerSchema returns the same id for schemas only differing in formatting") { client =>
    for {
      subject <- newSubject
      id <- client.registerSchema(subject, first)
      again <- client.registerSchema(subject, formatted)
      latest <- client.getSubjectVersion(subject, VersionSelector.Latest)
    } yield expect.eql(id, again) && expect.eql(SubjectVersion(1), latest.version)
  }

  test("registerSchema returns the same id for a schema in different subjects") { client =>
    for {
      subject <- newSubject
      other <- newSubject
      id <- client.registerSchema(subject, first)
      otherId <- client.registerSchema(other, first)
    } yield expect.eql(id, otherId)
  }

  test("registerSchema registers new versions of subjects") { client =>
    for {
      subject <- newSubject
      firstId <- client.registerSchema(subject, first)
      secondId <- client.registerSchema(subject, second)
      latest <- client.getSubjectVersion(subject, VersionSelector.Latest)
      initial <- client.getSubjectVersion(subject, VersionSelector.Exact(SubjectVersion(1)))
    } yield expect(firstId != secondId) &&
      expect.eql(subject, latest.subject) &&
      expect.eql(secondId, latest.id) &&
      expect.eql(SubjectVersion(2), latest.version) &&
      expectSameSchema(second, latest.schema) &&
      expect.eql(firstId, initial.id) &&
      expect.eql(SubjectVersion(1), initial.version) &&
      expectSameSchema(first, initial.schema)
  }

  test("registerSchema with normalize registers the schema") { client =>
    for {
      subject <- newSubject
      id <- client.registerSchema(subject, first, normalize = true)
      registered <- client.lookupSchema(subject, first, normalize = true)
    } yield expect.eql(Some(id), registered.map(_.id))
  }

  test("registerSchema raises an error for incompatible schemas") { client =>
    for {
      subject <- newSubject
      _ <- client.registerSchema(subject, first)
      result <- client.registerSchema(subject, incompatible).attempt
    } yield expectError(result, Status.Conflict, incompatibleSchemaErrorCode)
  }

  test("registerSchema raises an error for invalid schemas") { client =>
    for {
      subject <- newSubject
      result <- client.registerSchema(subject, invalid).attempt
    } yield expectError(result, Status.UnprocessableContent, SchemaRegistryErrorCode.InvalidSchema)
  }

  test("getSchema returns registered schemas") { client =>
    for {
      subject <- newSubject
      id <- client.registerSchema(subject, first)
      schema <- client.getSchema(id)
    } yield expectSameSchema(first, schema)
  }

  test("getSchema raises an error for missing schemas") { client =>
    client.getSchema(missingId).attempt.map { result =>
      expectError(result, Status.NotFound, SchemaRegistryErrorCode.SchemaNotFound)
    }
  }

  test("getSchemaOption returns registered schemas") { client =>
    for {
      subject <- newSubject
      id <- client.registerSchema(subject, first)
      schema <- client.getSchemaOption(id)
    } yield matches(schema) { case Some(found) => expectSameSchema(first, found) }
  }

  test("getSchemaOption returns None for missing schemas") { client =>
    client.getSchemaOption(missingId).map(schema => expect.eql(None, schema))
  }

  test("lookupSchema returns schemas registered under the subject") { client =>
    for {
      subject <- newSubject
      id <- client.registerSchema(subject, first)
      registered <- client.lookupSchema(subject, first)
    } yield matches(registered) { case Some(found) =>
      expect.eql(subject, found.subject) &&
      expect.eql(id, found.id) &&
      expect.eql(SubjectVersion(1), found.version) &&
      expectSameSchema(first, found.schema)
    }
  }

  test("lookupSchema returns None for missing subjects") { client =>
    for {
      subject <- newSubject
      registered <- client.lookupSchema(subject, first)
    } yield expect.eql(None, registered)
  }

  test("lookupSchema returns None for schemas not registered under the subject") { client =>
    for {
      subject <- newSubject
      other <- newSubject
      _ <- client.registerSchema(subject, first)
      _ <- client.registerSchema(other, second)
      registered <- client.lookupSchema(subject, second)
    } yield expect.eql(None, registered)
  }

  test("getSubjectVersion raises an error for missing subjects") { client =>
    for {
      subject <- newSubject
      result <- client.getSubjectVersion(subject, VersionSelector.Latest).attempt
    } yield expectError(result, Status.NotFound, SchemaRegistryErrorCode.SubjectNotFound)
  }

  test("getSubjectVersionOption returns None for missing subjects and versions") { client =>
    for {
      subject <- newSubject
      missingSubject <- client.getSubjectVersionOption(subject, VersionSelector.Latest)
      _ <- client.registerSchema(subject, first)
      missingVersion <- client.getSubjectVersionOption(subject, VersionSelector.Exact(SubjectVersion(2)))
      latest <- client.getSubjectVersionOption(subject, VersionSelector.Latest)
    } yield expect.eql(None, missingSubject) &&
      expect.eql(None, missingVersion) &&
      expect.eql(Some(SubjectVersion(1)), latest.map(_.version))
  }

  test("checkCompatibility returns whether schemas are compatible") { client =>
    for {
      subject <- newSubject
      _ <- client.registerSchema(subject, first)
      compatible <- client.checkCompatibility(subject, second)
      incompatible <- client.checkCompatibility(subject, SchemaRegistryClientSuite.incompatible)
      exact <- client.checkCompatibility(subject, second, VersionSelector.Exact(SubjectVersion(1)))
    } yield expect.all(
      clue(compatible).isCompatible,
      !clue(incompatible).isCompatible,
      clue(exact).isCompatible
    )
  }

  test("checkCompatibility raises an error for missing versions") { client =>
    for {
      subject <- newSubject
      missingSubject <- client
        .checkCompatibility(subject, first, VersionSelector.Exact(SubjectVersion(1)))
        .attempt
      _ <- client.registerSchema(subject, first)
      missingVersion <- client
        .checkCompatibility(subject, first, VersionSelector.Exact(SubjectVersion(2)))
        .attempt
    } yield expectError(missingSubject, Status.NotFound, SchemaRegistryErrorCode.VersionNotFound) &&
      expectError(missingVersion, Status.NotFound, SchemaRegistryErrorCode.VersionNotFound)
  }

  if (compatibleWithMissingSubjects) {
    test("checkCompatibility returns compatible for the latest version of missing subjects") { client =>
      for {
        subject <- newSubject
        compatibility <- client.checkCompatibility(subject, first)
      } yield expect.eql(Compatibility.Compatible, compatibility)
    }
  } else {
    test("checkCompatibility raises an error for the latest version of missing subjects") { client =>
      for {
        subject <- newSubject
        result <- client.checkCompatibility(subject, first).attempt
      } yield expectError(result, Status.NotFound, SchemaRegistryErrorCode.VersionNotFound)
    }
  }

  if (supportsGuids) {
    test("getSchemaByGuid returns schemas by the GUID of registered versions") { client =>
      for {
        subject <- newSubject
        _ <- client.registerSchema(subject, first)
        latest <- client.getSubjectVersion(subject, VersionSelector.Latest)
        guid <- matchOrFailFast[IO](latest.guid) { case Some(value) => value }
        schema <- client.getSchemaByGuid(guid)
        option <- client.getSchemaByGuidOption(guid)
      } yield expectSameSchema(first, schema) &&
        matches(option) { case Some(found) => expectSameSchema(first, found) }
    }

    test("getSchemaByGuidOption returns None for missing GUIDs") { client =>
      client.getSchemaByGuidOption(missingGuid).map(schema => expect.eql(None, schema))
    }
  }
}

object SchemaRegistryClientSuite {

  /** Avro record schema, in the format the Java Avro library prints schemas. */
  val first: Schema =
    Schema(
      """{"type":"record","name":"Test","namespace":"org.http4s.schemaregistry","fields":[{"name":"name","type":"string"}]}"""
    )

  /** The same schema as [[first]], with different formatting. */
  val formatted: Schema =
    Schema(
      """{
        |  "type": "record",
        |  "name": "Test",
        |  "namespace": "org.http4s.schemaregistry",
        |  "fields": [{ "name": "name", "type": "string" }]
        |}""".stripMargin
    )

  /** Schema which is backward compatible with [[first]], adding a field with a default. */
  val second: Schema =
    Schema(
      """{"type":"record","name":"Test","namespace":"org.http4s.schemaregistry","fields":[{"name":"name","type":"string"},{"name":"age","type":"int","default":0}]}"""
    )

  /** Schema which is not backward compatible with [[first]], changing the type of a field. */
  val incompatible: Schema =
    Schema(
      """{"type":"record","name":"Test","namespace":"org.http4s.schemaregistry","fields":[{"name":"name","type":"int"}]}"""
    )

  /** Schema with an invalid definition. */
  val invalid: Schema =
    Schema("""{"type":""")

  /** Schema id which is not used by tests. */
  val missingId: SchemaId =
    SchemaId(Int.MaxValue)

  /** Schema GUID which is not used by tests. */
  val missingGuid: SchemaGuid =
    SchemaGuid("ffffffff-ffff-ffff-ffff-ffffffffffff")

  /** Counter for subject names, which are unique for each test run. */
  private val subjects: AtomicLong =
    new AtomicLong(0L)
}
