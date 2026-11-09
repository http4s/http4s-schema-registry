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
import org.http4s.Status
import org.http4s.schemaregistry.Generators.*
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object ModelSuite extends SimpleIOSuite with Checkers {
  val schema: Schema =
    Schema("\"string\"")

  val reference: SchemaReference =
    SchemaReference("Other", SubjectName("other"), SubjectVersion(2))

  val registered: RegisteredSchema =
    RegisteredSchema(SubjectName("subject"), SchemaId(1), SubjectVersion(3), schema)

  pureTest("values are equal when their contents are equal") {
    val error =
      SchemaRegistryError(Status.NotFound, Some(SchemaRegistryErrorCode.SchemaNotFound), "Schema 1 not found")
    val sameError =
      SchemaRegistryError(Status.NotFound, Some(SchemaRegistryErrorCode(40403)), "Schema 1 not found")

    expect.eql(SchemaId(1), SchemaId(1)) &&
    expect(SchemaId(1) != SchemaId(2)) &&
    expect.eql(SchemaGuid("guid"), SchemaGuid("guid")) &&
    expect(SchemaGuid("guid") != SchemaGuid("other")) &&
    expect.eql(SubjectName("subject"), SubjectName("subject")) &&
    expect.eql(SubjectVersion(1), SubjectVersion(1)) &&
    expect.eql(SchemaType("AVRO"), SchemaType.Avro) &&
    expect.eql(VersionSelector.Exact(SubjectVersion(1)), VersionSelector.Exact(SubjectVersion(1))) &&
    expect(VersionSelector.Exact(SubjectVersion(1)) != VersionSelector.Latest) &&
    expect.eql(SchemaReference("Other", SubjectName("other"), SubjectVersion(2)), reference) &&
    expect.eql(Schema("\"string\""), schema) &&
    expect.eql(
      RegisteredSchema(SubjectName("subject"), SchemaId(1), SubjectVersion(3), schema),
      registered
    ) &&
    expect.eql(Compatibility.Incompatible(List("error")), Compatibility.Incompatible(List("error"))) &&
    expect(Compatibility.Incompatible(List("error")) != Compatibility.Incompatible(Nil)) &&
    expect(Compatibility.Compatible != Compatibility.Incompatible(Nil)) &&
    expect.eql(SchemaRegistryErrorCode(40401), SchemaRegistryErrorCode.SubjectNotFound) &&
    expect(SchemaRegistryErrorCode.SubjectNotFound != SchemaRegistryErrorCode.SchemaNotFound) &&
    expect.eql(sameError, error) &&
    expect.eql(sameError.hashCode, error.hashCode) &&
    expect(
      error != SchemaRegistryError(Status.NotFound, Some(SchemaRegistryErrorCode(40401)), error.message)
    ) &&
    expect(error != SchemaRegistryError(Status.NotFound, None, error.message))
  }

  test("schema ids and subject versions are ordered by their values") {
    forall(Gen.zip(schemaIds, schemaIds, subjectVersions, subjectVersions)) {
      case (id, otherId, version, otherVersion) =>
        expect.eql(Integer.compare(id.value, otherId.value), Order[SchemaId].compare(id, otherId)) &&
        expect.eql(
          Integer.compare(version.value, otherVersion.value),
          Order[SubjectVersion].compare(version, otherVersion)
        ) &&
        expect.eql(id.hashCode, Hash[SchemaId].hash(id)) &&
        expect.eql(version.hashCode, Hash[SubjectVersion].hash(version))
    }
  }

  test("schemas are Avro schemas without references, unless another type and references are specified") {
    forall(Gen.zip(stringsWithSpecialCharacters, schemaTypes, listsOfUpToThree(schemaReferences))) {
      case (definition, schemaType, references) =>
        val schema = Schema(definition)
        val specified = schema.withSchemaType(schemaType).withReferences(references)
        expect.eql(definition, schema.definition) &&
        expect.eql(SchemaType.Avro, schema.schemaType) &&
        expect.eql(Nil, schema.references) &&
        expect.eql(definition, specified.definition) &&
        expect.eql(schemaType, specified.schemaType) &&
        expect.eql(references, specified.references)
    }
  }

  test("registered schemas do not have GUIDs, unless a GUID is specified") {
    val registrations = Gen.zip(subjectNames, schemaIds, subjectVersions, schemas)
    forall(Gen.zip(registrations, schemaGuids, schemaGuids)) {
      case ((subject, id, version, schema), guid, otherGuid) =>
        val registered = RegisteredSchema(subject, id, version, schema)
        val withGuid = registered.withGuid(guid)
        expect.eql(subject, registered.subject) &&
        expect.eql(id, registered.id) &&
        expect.eql(version, registered.version) &&
        expect.eql(schema, registered.schema) &&
        expect.eql(None, registered.guid) &&
        expect.eql(Some(guid), withGuid.guid) &&
        expect.eql(Some(otherGuid), withGuid.withGuid(otherGuid).guid) &&
        expect.eql(registered, withGuid.withoutGuid)
    }
  }

  test("version selectors select the latest version or an exact version") {
    forall(subjectVersions) { version =>
      expect.eql(None, VersionSelector.Latest.exactVersion) &&
      expect.eql(Some(version), VersionSelector.Exact(version).exactVersion)
    }
  }

  test("compatibility is either compatible without errors, or incompatible with errors") {
    forall(listsOfUpToThree(stringsWithSpecialCharacters)) { errors =>
      val incompatible = Compatibility.Incompatible(errors)
      expect(Compatibility.Compatible.isCompatible) &&
      expect.eql(Nil, Compatibility.Compatible.errors) &&
      expect(!incompatible.isCompatible) &&
      expect.eql(errors, incompatible.errors)
    }
  }

  test("errors include the status, error code and message, without a stack trace") {
    forall(Gen.zip(statuses, Gen.option(errorCodes), stringsWithSpecialCharacters)) {
      case (status, errorCode, message) =>
        val error = SchemaRegistryError(status, errorCode, message)
        expect.eql(status, error.status) &&
        expect.eql(errorCode, error.errorCode) &&
        expect.eql(message, error.message) &&
        expect(error.getStackTrace.isEmpty)
    }
  }

  pureTest("error messages include the status, the error code if there is one, and the message") {
    expect.eql(
      "Schema registry responded with status 404 and error code 40403: Schema 1 not found",
      SchemaRegistryError(
        Status.NotFound,
        Some(SchemaRegistryErrorCode.SchemaNotFound),
        "Schema 1 not found"
      ).getMessage
    ) &&
    expect.eql(
      "Schema registry responded with status 502: Bad gateway",
      SchemaRegistryError(Status.BadGateway, None, "Bad gateway").getMessage
    )
  }

  pureTest("toString includes the contents of values") {
    expect.eql("SchemaId(1)", SchemaId(1).toString) &&
    expect.eql("SchemaGuid(guid)", SchemaGuid("guid").toString) &&
    expect.eql("SubjectName(subject)", SubjectName("subject").toString) &&
    expect.eql("SubjectVersion(2)", SubjectVersion(2).toString) &&
    expect.eql("SchemaType(AVRO)", SchemaType.Avro.toString) &&
    expect.eql("Latest", VersionSelector.Latest.toString) &&
    expect.eql("Exact(SubjectVersion(2))", VersionSelector.Exact(SubjectVersion(2)).toString) &&
    expect.eql(
      "SchemaReference(name = Other, subject = SubjectName(other), version = SubjectVersion(2))",
      reference.toString
    ) &&
    expect.eql(
      "Schema(definition = \"string\", schemaType = SchemaType(AVRO), references = List())",
      schema.toString
    ) &&
    expect.eql(
      "RegisteredSchema(subject = SubjectName(subject), id = SchemaId(1), version = SubjectVersion(3), schema = Schema(definition = \"string\", schemaType = SchemaType(AVRO), references = List()), guid = Some(SchemaGuid(guid)))",
      registered.withGuid(SchemaGuid("guid")).toString
    ) &&
    expect.eql("Compatible", Compatibility.Compatible.toString) &&
    expect.eql("Incompatible(List(error))", Compatibility.Incompatible(List("error")).toString) &&
    expect.eql("SchemaRegistryErrorCode(40401)", SchemaRegistryErrorCode.SubjectNotFound.toString) &&
    expect.eql(
      "SchemaRegistryError(status = 404 Not Found, errorCode = Some(SchemaRegistryErrorCode(40403)), message = Schema 1 not found)",
      SchemaRegistryError(
        Status.NotFound,
        Some(SchemaRegistryErrorCode.SchemaNotFound),
        "Schema 1 not found"
      ).toString
    ) &&
    expect.eql(
      "SchemaRegistryError(status = 502 Bad Gateway, errorCode = None, message = Bad gateway)",
      SchemaRegistryError(Status.BadGateway, None, "Bad gateway").toString
    )
  }
}
