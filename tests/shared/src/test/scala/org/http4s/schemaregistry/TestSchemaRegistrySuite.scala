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
import cats.syntax.all.*
import io.circe.Printer
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.schemaregistry.Generators.*
import org.http4s.schemaregistry.SchemaRegistryClientSuite.first
import org.http4s.schemaregistry.SchemaRegistryClientSuite.second
import org.http4s.syntax.literals.*
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object TestSchemaRegistrySuite extends SimpleIOSuite with Checkers {
  val third: Schema =
    Schema("\"string\"")

  test("registered returns the versions of all subjects in registration order") {
    for {
      registry <- TestSchemaRegistry[IO]
      client <- registry.client.build
      firstId <- client.registerSchema(SubjectName("first"), first)
      secondId <- client.registerSchema(SubjectName("second"), second)
      _ <- client.registerSchema(SubjectName("first"), second)
      registered <- registry.registered
    } yield expect.eql(
      List(("first", firstId, 1), ("second", secondId, 1), ("first", secondId, 2)),
      registered.map(registered => (registered.subject.value, registered.id, registered.version.value))
    )
  }

  test("compatibility is checked against all versions when registering, and the version when checking") {
    val compatibility: (SubjectName, Schema, List[RegisteredSchema]) => Compatibility =
      (_, schema, versions) =>
        if (schema == third)
          Compatibility.Incompatible(List(s"versions: ${versions.map(_.version.value).mkString(",")}"))
        else Compatibility.Compatible

    for {
      registry <- TestSchemaRegistry.withCompatibility[IO](compatibility)
      client <- registry.client.build
      subject = SubjectName("subject")
      _ <- client.registerSchema(subject, first)
      _ <- client.registerSchema(subject, second)
      result <- client.registerSchema(subject, third).attempt
      checked <- client.checkCompatibility(subject, third, VersionSelector.Exact(SubjectVersion(1)))
    } yield matches(result) { case Left(error: SchemaRegistryError) =>
      expect.same(Status.Conflict, error.status) &&
      expect(clue(error.message).endsWith("details: versions: 1,2"))
    } && expect.eql(Compatibility.Incompatible(List("versions: 1")), checked)
  }

  test("schemas only differing in the formatting of their definitions are the same schema") {
    val printers = List(Printer.noSpaces, Printer.spaces2, Printer.spaces4SortKeys)
    forall(jsonValues) { json =>
      for {
        registry <- TestSchemaRegistry[IO]
        client <- registry.client.build
        ids <- printers.traverse(printer =>
          client.registerSchema(SubjectName("subject"), Schema(printer.print(json)))
        )
        registered <- registry.registered
      } yield expect.eql(1, ids.distinct.size) && expect.eql(1, registered.size)
    }
  }

  test("unknown routes respond with not found") {
    for {
      registry <- TestSchemaRegistry[IO]
      response <- registry.httpApp.run(Request[IO](Method.GET, uri"/config"))
    } yield expect.same(Status.NotFound, response.status)
  }

  test("invalid versions respond with an error") {
    for {
      registry <- TestSchemaRegistry[IO]
      client <- registry.client.build
      _ <- client.registerSchema(SubjectName("subject"), first)
      responses <- List("first", "0", "-2", "2147483648").traverse { version =>
        registry.httpApp.run(Request[IO](Method.GET, uri"/subjects/subject/versions" / version))
      }
      bodies <- responses.traverse(_.as[String])
    } yield forEach(responses.zip(bodies)) { case (response, body) =>
      expect.same(Status.UnprocessableContent, response.status) &&
      expect(clue(body).contains(SchemaRegistryErrorCode.InvalidVersion.value.show))
    }
  }
}
