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

import cats.Eq
import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.jawn
import io.circe.syntax.*
import org.http4s.schemaregistry.Generators.*
import org.scalacheck.Gen
import weaver.Expectations
import weaver.SimpleIOSuite
import weaver.SourceLocation
import weaver.scalacheck.Checkers

object JsonSuite extends SimpleIOSuite with Checkers {
  val reference: SchemaReference =
    SchemaReference("other.proto", SubjectName("other"), SubjectVersion(1))

  val avro: Schema =
    Schema("\"string\"")

  val protobuf: Schema =
    Schema("syntax = \"proto3\";").withSchemaType(SchemaType.Protobuf).withReferences(List(reference))

  val registered: RegisteredSchema =
    RegisteredSchema(SubjectName("subject"), SchemaId(3), SubjectVersion(2), protobuf)
      .withGuid(SchemaGuid("00000000-0000-0000-0000-000000000003"))

  def encodes[A: Encoder](json: String, a: A)(implicit loc: SourceLocation): Expectations =
    matches(jawn.parse(json)) { case Right(expected) => expect.eql(expected, a.asJson) }

  def decodes[A: Decoder: Eq: Show](json: String, a: A)(implicit loc: SourceLocation): Expectations =
    matches(jawn.decode[A](json)) { case Right(decoded) => expect.eql(a, decoded) }

  def roundtrips[A: Encoder: Decoder: Eq: Show](values: Gen[A])(implicit
    loc: SourceLocation
  ): IO[Expectations] =
    forall(values)(a => decodes(a.asJson.noSpaces, a))

  pureTest("values are encoded as in requests and responses of schema registries") {
    encodes("1", SchemaId(1)) &&
    encodes("\"00000000-0000-0000-0000-000000000001\"", SchemaGuid("00000000-0000-0000-0000-000000000001")) &&
    encodes("\"subject\"", SubjectName("subject")) &&
    encodes("2", SubjectVersion(2)) &&
    encodes("\"PROTOBUF\"", SchemaType.Protobuf) &&
    encodes("40401", SchemaRegistryErrorCode.SubjectNotFound) &&
    encodes("""{"name":"other.proto","subject":"other","version":1}""", reference) &&
    encodes("""{"schema":"\"string\""}""", avro) &&
    encodes(
      """{"schema":"syntax = \"proto3\";","schemaType":"PROTOBUF","references":[{"name":"other.proto","subject":"other","version":1}]}""",
      protobuf
    ) &&
    encodes(
      """{"subject":"subject","version":2,"id":3,"schema":"syntax = \"proto3\";","schemaType":"PROTOBUF","references":[{"name":"other.proto","subject":"other","version":1}],"guid":"00000000-0000-0000-0000-000000000003"}""",
      registered
    ) &&
    encodes(
      """{"subject":"subject","version":2,"id":3,"schema":"\"string\""}""",
      RegisteredSchema(SubjectName("subject"), SchemaId(3), SubjectVersion(2), avro)
    ) &&
    encodes("""{"is_compatible":true,"messages":[]}""", Compatibility.Compatible) &&
    encodes("""{"is_compatible":false,"messages":["error"]}""", Compatibility.Incompatible(List("error")))
  }

  test("values are decoded from the JSON they are encoded as") {
    List(
      roundtrips(schemaIds),
      roundtrips(schemaGuids),
      roundtrips(subjectNames),
      roundtrips(subjectVersions),
      roundtrips(schemaTypes),
      roundtrips(errorCodes),
      roundtrips(schemaReferences),
      roundtrips(schemas),
      roundtrips(registeredSchemas),
      roundtrips(compatibilities)
    ).sequence.map(_.combineAll)
  }

  pureTest("schemas without a type and references are decoded as Avro schemas without references") {
    decodes("""{"schema":"\"string\""}""", avro) &&
    decodes("""{"schema":"\"string\"","schemaType":null,"references":null}""", avro) &&
    decodes("""{"schema":"\"string\"","schemaType":"AVRO","references":[]}""", avro)
  }

  pureTest("registered schemas are decoded ignoring unknown fields") {
    decodes(
      """{"subject":"subject","version":2,"id":3,"schema":"\"string\"","ts":1,"deleted":false}""",
      RegisteredSchema(SubjectName("subject"), SchemaId(3), SubjectVersion(2), avro)
    )
  }

  pureTest("compatibility is decoded with messages as errors only if incompatible, printing non-strings") {
    decodes("""{"is_compatible":true}""", Compatibility.Compatible) &&
    decodes("""{"is_compatible":true,"messages":["ignored"]}""", Compatibility.Compatible) &&
    decodes("""{"is_compatible":false}""", Compatibility.Incompatible(Nil)) &&
    decodes(
      """{"is_compatible":false,"messages":["first",{"second":2}]}""",
      Compatibility.Incompatible(List("first", """{"second":2}"""))
    )
  }
}
