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

import cats.Show
import cats.data.EitherT
import cats.effect.IO
import cats.effect.Resource
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import io.circe.Json
import io.circe.jawn
import io.circe.syntax.*
import java.util.UUID
import org.http4s.Headers
import org.http4s.HttpApp
import org.http4s.InvalidMessageBodyFailure
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.middleware.FollowRedirect
import org.http4s.headers.Location
import org.http4s.schemaregistry.Generators.*
import org.http4s.syntax.literals.*
import org.scalacheck.Gen
import org.typelevel.ci.CIString
import scala.concurrent.duration.*
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object SchemaRegistryClientBuilderSuite extends SimpleIOSuite with Checkers {
  final case class Recorded(method: Method, uri: Uri, headers: Headers, body: String) {
    def header(name: String): Option[String] =
      headers.get(CIString(name)).map(_.head.value)

    def json: Json =
      jawn.parse(body).fold(throw _, identity)
  }

  final case class Recording(app: HttpApp[IO], requests: IO[List[Recorded]])

  val schema: Schema =
    Schema("\"string\"")

  val schemaResponse: String =
    """{"schema":"\"string\""}"""

  val registeredResponse: String =
    """{"subject":"subject","version":1,"id":1,"schema":"\"string\""}"""

  def respond(status: org.http4s.Status, body: String): Response[IO] =
    Response[IO](status).withEntity(body)

  def recording(response: Request[IO] => IO[Response[IO]]): IO[Recording] =
    IO.ref(List.empty[Recorded]).map { requests =>
      val app = HttpApp[IO] { request =>
        request.as[String].flatMap { body =>
          val recorded = Recorded(request.method, request.uri, request.headers, body)
          requests.update(_ :+ recorded) >> response(request)
        }
      }

      Recording(app, requests.get)
    }

  def clientOf(
    app: HttpApp[IO],
    uri: Uri = uri"http://localhost:8081"
  )(configure: SchemaRegistryClientBuilder[IO] => SchemaRegistryClientBuilder[IO] = identity) =
    configure(SchemaRegistryClientBuilder.default(Client.fromHttpApp(app), uri)).build

  def requestsOf(
    uri: Uri = uri"http://localhost:8081",
    response: String = schemaResponse
  )(request: SchemaRegistryClient[IO] => IO[Any]): IO[List[Recorded]] =
    for {
      recorded <- recording(_ => IO.pure(respond(Status.Ok, response)))
      client <- clientOf(recorded.app, uri)()
      _ <- request(client)
      requests <- recorded.requests
    } yield requests

  def clientResponding(status: org.http4s.Status, body: String): IO[SchemaRegistryClient[IO]] =
    recording(_ => IO.pure(respond(status, body))).flatMap(recorded => clientOf(recorded.app)())

  test("requests are sent to paths under the path of the base uri") {
    val uris =
      List(
        uri"http://localhost:8081",
        uri"http://localhost:8081/",
        uri"http://localhost:8081/registry",
        uri"http://localhost:8081/registry/"
      )

    uris
      .traverse(uri => requestsOf(uri)(_.getSchema(SchemaId(1))).map(_.map(_.uri.renderString)))
      .map { found =>
        expect.eql(
          List(
            List("http://localhost:8081/schemas/ids/1"),
            List("http://localhost:8081/schemas/ids/1"),
            List("http://localhost:8081/registry/schemas/ids/1"),
            List("http://localhost:8081/registry/schemas/ids/1")
          ),
          found
        )
      }
  }

  test("requests are sent to the paths of the endpoints") {
    val subject = SubjectName("a/b c")
    val guid = SchemaGuid("00000000-0000-0000-0000-000000000001")
    val requests =
      List(
        requestsOf()(_.getSchemaByGuid(guid)),
        requestsOf(response = """{"id":1}""")(_.registerSchema(subject, schema)),
        requestsOf(response = """{"id":1}""")(_.registerSchema(subject, schema, normalize = true)),
        requestsOf(response = registeredResponse)(_.lookupSchema(subject, schema)),
        requestsOf(response = registeredResponse)(_.lookupSchema(subject, schema, normalize = true)),
        requestsOf(response = registeredResponse)(_.getSubjectVersion(subject, VersionSelector.Latest)),
        requestsOf(response = registeredResponse)(
          _.getSubjectVersion(subject, VersionSelector.Exact(SubjectVersion(3)))
        ),
        requestsOf(response = """{"is_compatible":true}""")(_.checkCompatibility(subject, schema)),
        requestsOf(response = """{"is_compatible":true}""")(
          _.checkCompatibility(subject, schema, VersionSelector.Exact(SubjectVersion(3)))
        )
      ).sequence

    requests.map { requests =>
      expect.eql(
        List(
          "GET http://localhost:8081/schemas/guids/00000000-0000-0000-0000-000000000001",
          "POST http://localhost:8081/subjects/a%2Fb%20c/versions",
          "POST http://localhost:8081/subjects/a%2Fb%20c/versions?normalize=true",
          "POST http://localhost:8081/subjects/a%2Fb%20c",
          "POST http://localhost:8081/subjects/a%2Fb%20c?normalize=true",
          "GET http://localhost:8081/subjects/a%2Fb%20c/versions/latest",
          "GET http://localhost:8081/subjects/a%2Fb%20c/versions/3",
          "POST http://localhost:8081/compatibility/subjects/a%2Fb%20c/versions/latest?verbose=true",
          "POST http://localhost:8081/compatibility/subjects/a%2Fb%20c/versions/3?verbose=true"
        ),
        requests.flatten.map(request => s"${request.method} ${request.uri.renderString}")
      )
    }
  }

  test("subjects are sent as path segments, regardless of their characters") {
    forall(subjectNames) { subject =>
      requestsOf(response = registeredResponse)(_.getSubjectVersion(subject, VersionSelector.Latest)).map {
        requests =>
          matches(requests.map(request => Uri.fromString(request.uri.renderString))) {
            case List(Right(uri)) =>
              expect.eql(
                List("subjects", subject.value, "versions", "latest"),
                uri.path.segments.map(_.decoded()).toList
              )
          }
      }
    }
  }

  test("requests include headers for the media types and extended properties") {
    requestsOf(response = """{"id":1}""")(_.registerSchema(SubjectName("subject"), schema)).map { requests =>
      matches(requests) { case List(request) =>
        expect.eql(
          Some(
            "application/vnd.schemaregistry.v1+json, application/vnd.schemaregistry+json;q=0.9, application/json;q=0.5"
          ),
          request.header("Accept")
        ) &&
        expect.eql(Some("application/vnd.schemaregistry.v1+json"), request.header("Content-Type")) &&
        expect.eql(Some("true"), request.header("Confluent-Accept-Unknown-Properties")) &&
        expect.eql(None, request.header("Authorization"))
      }
    }
  }

  test("requests include the authorization of the auth") {
    val auths =
      List(Auth.Basic("username", "password"), Auth.Bearer("token"), Auth.None)

    auths
      .traverse { auth =>
        for {
          recorded <- recording(_ => IO.pure(respond(Status.Ok, schemaResponse)))
          client <- clientOf(recorded.app)(_.withAuth(auth))
          _ <- client.getSchema(SchemaId(1))
          requests <- recorded.requests
        } yield requests.map(_.header("Authorization"))
      }
      .map { found =>
        expect.eql(
          List(List(Some("Basic dXNlcm5hbWU6cGFzc3dvcmQ=")), List(Some("Bearer token")), List(None)),
          found
        )
      }
  }

  test("request bodies include the schema type and references when needed") {
    val protobuf =
      Schema("syntax = \"proto3\";")
        .withSchemaType(SchemaType.Protobuf)
        .withReferences(List(SchemaReference("other.proto", SubjectName("other"), SubjectVersion(2))))

    List(schema, protobuf)
      .traverse(schema =>
        requestsOf(response = """{"id":1}""")(_.registerSchema(SubjectName("subject"), schema))
      )
      .map { requests =>
        expect.eql(
          List(
            Json.obj("schema" -> Json.fromString("\"string\"")),
            Json.obj(
              "schema" -> Json.fromString("syntax = \"proto3\";"),
              "schemaType" -> Json.fromString("PROTOBUF"),
              "references" -> Json.arr(
                Json.obj(
                  "name" -> Json.fromString("other.proto"),
                  "subject" -> Json.fromString("other"),
                  "version" -> Json.fromInt(2)
                )
              )
            )
          ),
          requests.flatten.map(_.json)
        )
      }
  }

  test("responses are decoded including the schema type, references and GUID") {
    val response =
      """{"subject":"subject","version":2,"id":3,"schema":"syntax = \"proto3\";","schemaType":"PROTOBUF","references":[{"name":"other.proto","subject":"other","version":1}],"guid":"00000000-0000-0000-0000-000000000003","ts":1,"deleted":false}"""

    for {
      recorded <- recording(_ => IO.pure(respond(Status.Ok, response)))
      client <- clientOf(recorded.app)()
      registered <- client.getSubjectVersion(SubjectName("subject"), VersionSelector.Latest)
    } yield expect.eql(
      RegisteredSchema(
        SubjectName("subject"),
        SchemaId(3),
        SubjectVersion(2),
        Schema("syntax = \"proto3\";")
          .withSchemaType(SchemaType.Protobuf)
          .withReferences(List(SchemaReference("other.proto", SubjectName("other"), SubjectVersion(1))))
      ).withGuid(SchemaGuid("00000000-0000-0000-0000-000000000003")),
      registered
    )
  }

  test("compatibility messages are decoded as errors, printing messages which are not strings") {
    val response = """{"is_compatible":false,"messages":["first",{"second":2}]}"""
    for {
      recorded <- recording(_ => IO.pure(respond(Status.Ok, response)))
      client <- clientOf(recorded.app)()
      compatibility <- client.checkCompatibility(SubjectName("subject"), schema)
    } yield expect.eql(Compatibility.Incompatible(List("first", """{"second":2}""")), compatibility)
  }

  test("error responses are raised with their error code and message") {
    forall(Gen.zip(unsuccessfulStatuses, errorCodes, stringsWithSpecialCharacters)) {
      case (status, errorCode, message) =>
        val body = Json.obj("error_code" -> errorCode.asJson, "message" -> message.asJson).noSpaces
        for {
          client <- clientResponding(status, body)
          result <- client.getSchema(SchemaId(1)).attempt
        } yield expect.same(Left(SchemaRegistryError(status, Some(errorCode), message)), result)
    }
  }

  test("error responses without error codes are raised without an error code, and with the body") {
    forall(Gen.zip(unsuccessfulStatuses, stringsWithSpecialCharacters)) { case (status, body) =>
      for {
        client <- clientResponding(status, body)
        result <- client.getSchema(SchemaId(1)).attempt
      } yield expect.same(Left(SchemaRegistryError(status, None, body.trim)), result)
    }
  }

  test("redirects are raised as errors, unless the client follows redirects") {
    val subject = SubjectName("subject")
    for {
      recorded <- recording { request =>
        IO.pure {
          if (request.uri.path.renderString.startsWith("/registry")) respond(Status.Ok, """{"id":1}""")
          else
            Response[IO](Status.PermanentRedirect)
              .putHeaders(Location(uri"/registry/subjects/subject/versions"))
        }
      }
      client <- clientOf(recorded.app)()
      redirected <- client.registerSchema(subject, schema).attempt
      following <- SchemaRegistryClientBuilder
        .default(
          FollowRedirect(maxRedirects = 1)(Client.fromHttpApp(recorded.app)),
          uri"http://localhost:8081"
        )
        .build
      id <- following.registerSchema(subject, schema)
      requests <- recorded.requests
    } yield expect.same(Left(SchemaRegistryError(Status.PermanentRedirect, None, "")), redirected) &&
      expect.eql(SchemaId(1), id) &&
      expect.eql(
        List(
          "POST /subjects/subject/versions",
          "POST /subjects/subject/versions",
          "POST /registry/subjects/subject/versions"
        ),
        requests.map(request => s"${request.method} ${request.uri.path.renderString}")
      ) &&
      forEach(requests)(request => expect.eql(schema.asJson, request.json))
  }

  test("successful responses which cannot be decoded are raised as invalid message bodies") {
    for {
      recorded <- recording(_ => IO.pure(respond(Status.Ok, """{"unexpected":true}""")))
      client <- clientOf(recorded.app)()
      result <- client.getSchema(SchemaId(1)).attempt
    } yield matches(result) { case Left(_: InvalidMessageBodyFailure) => success }
  }

  test("option variants return None only for not found errors with the expected error codes") {
    val subject = SubjectName("subject")
    for {
      schemaNotFound <- clientResponding(Status.NotFound, """{"error_code":40403,"message":"Not found"}""")
      subjectNotFound <- clientResponding(Status.NotFound, """{"error_code":40401,"message":"Not found"}""")
      versionNotFound <- clientResponding(Status.NotFound, """{"error_code":40402,"message":"Not found"}""")
      routeNotFound <- clientResponding(Status.NotFound, "Not found")
      missing <- List[IO[Option[Any]]](
        schemaNotFound.getSchemaOption(SchemaId(1)),
        schemaNotFound.getSchemaByGuidOption(SchemaGuid("00000000-0000-0000-0000-000000000001")),
        schemaNotFound.lookupSchema(subject, schema),
        subjectNotFound.lookupSchema(subject, schema),
        subjectNotFound.getSubjectVersionOption(subject, VersionSelector.Latest),
        versionNotFound.getSubjectVersionOption(subject, VersionSelector.Latest)
      ).sequence
      errors <- List[IO[Option[Any]]](
        versionNotFound.getSchemaOption(SchemaId(1)),
        routeNotFound.getSchemaOption(SchemaId(1)),
        routeNotFound.lookupSchema(subject, schema)
      ).traverse(_.attempt)
    } yield expect.same(List.fill(6)(None), missing) &&
      forEach(errors)(error => matches(error) { case Left(_: SchemaRegistryError) => success })
  }

  test("schemas and registrations are cached") {
    for {
      recorded <- recording { request =>
        val body = if (request.method == Method.GET) schemaResponse else """{"id":1}"""
        IO.pure(respond(Status.Ok, body))
      }
      client <- clientOf(recorded.app)()
      subject = SubjectName("subject")
      _ <- client.getSchema(SchemaId(1)).replicateA_(2)
      _ <- client.getSchemaByGuid(SchemaGuid("00000000-0000-0000-0000-000000000001")).replicateA_(2)
      _ <- client.registerSchema(subject, schema).replicateA_(2)
      _ <- client.registerSchema(subject, schema, normalize = true).replicateA_(2)
      requests <- recorded.requests
    } yield expect.eql(4, requests.size)
  }

  test("schemas and registrations are not cached without caching") {
    for {
      recorded <- recording(_ => IO.pure(respond(Status.Ok, schemaResponse)))
      client <- clientOf(recorded.app)(_.withCaching(false))
      _ <- client.getSchema(SchemaId(1)).replicateA_(3)
      requests <- recorded.requests
    } yield expect.eql(3, requests.size)
  }

  test("versions of subjects are cached, where latest versions are cached until they expire") {
    val subject = SubjectName("subject")
    val exact = VersionSelector.Exact(SubjectVersion(1))

    TestControl.executeEmbed {
      for {
        recorded <- recording(_ => IO.pure(respond(Status.Ok, registeredResponse)))
        client <- clientOf(recorded.app)()
        expiring <- clientOf(recorded.app)(_.withLatestCacheExpiry(50.millis))
        uncached <- clientOf(recorded.app)(_.withLatestCacheExpiry(Duration.Zero))
        _ <- client.getSubjectVersion(subject, VersionSelector.Latest).replicateA_(2)
        _ <- client.getSubjectVersion(subject, exact).replicateA_(2)
        cached <- recorded.requests
        _ <- expiring.getSubjectVersion(subject, VersionSelector.Latest)
        _ <- IO.sleep(49.millis)
        _ <- expiring.getSubjectVersion(subject, VersionSelector.Latest)
        _ <- IO.sleep(1.millis)
        _ <- expiring.getSubjectVersion(subject, VersionSelector.Latest)
        _ <- client.getSubjectVersion(subject, VersionSelector.Latest)
        expired <- recorded.requests
        _ <- uncached.getSubjectVersion(subject, VersionSelector.Latest).replicateA_(2)
        requests <- recorded.requests
      } yield expect.eql(
        List("/subjects/subject/versions/latest", "/subjects/subject/versions/1"),
        cached.map(_.uri.path.renderString)
      ) && expect.eql(4, expired.size) && expect.eql(6, requests.size)
    }
  }

  test("exact versions before version 1 are not cached, since registries can treat them as latest versions") {
    val subject = SubjectName("subject")
    for {
      recorded <- recording(_ => IO.pure(respond(Status.Ok, registeredResponse)))
      client <- clientOf(recorded.app)()
      _ <- client.getSubjectVersion(subject, VersionSelector.Exact(SubjectVersion(-1))).replicateA_(2)
      _ <- client.getSubjectVersion(subject, VersionSelector.Exact(SubjectVersion(1))).replicateA_(2)
      requests <- recorded.requests
    } yield expect.eql(
      List("/subjects/subject/versions/-1", "/subjects/subject/versions/-1", "/subjects/subject/versions/1"),
      requests.map(_.uri.path.renderString)
    )
  }

  test("lookups are cached only for registered schemas") {
    for {
      count <- IO.ref(0)
      recorded <- recording { _ =>
        count.getAndUpdate(_ + 1).map {
          case 0 => respond(Status.NotFound, """{"error_code":40403,"message":"Schema not found"}""")
          case _ => respond(Status.Ok, registeredResponse)
        }
      }
      client <- clientOf(recorded.app)()
      subject = SubjectName("subject")
      missing <- client.lookupSchema(subject, schema)
      registered <- client.lookupSchema(subject, schema)
      cached <- client.lookupSchema(subject, schema)
      requests <- recorded.requests
    } yield expect.eql(None, missing) &&
      expect.eql(Some(SchemaId(1)), registered.map(_.id)) &&
      expect.eql(registered, cached) &&
      expect.eql(2, requests.size)
  }

  test("failures are not cached") {
    for {
      count <- IO.ref(0)
      recorded <- recording { _ =>
        count.getAndUpdate(_ + 1).map {
          case 0 => respond(Status.InternalServerError, """{"error_code":50001,"message":"Error"}""")
          case _ => respond(Status.Ok, schemaResponse)
        }
      }
      client <- clientOf(recorded.app)()
      failed <- client.getSchema(SchemaId(1)).attempt
      found <- client.getSchema(SchemaId(1))
      requests <- recorded.requests
    } yield expect.same(
      Left(SchemaRegistryError(Status.InternalServerError, Some(SchemaRegistryErrorCode(50001)), "Error")),
      failed
    ) &&
      expect.eql(schema, found) &&
      expect.eql(2, requests.size)
  }

  test("concurrent requests for the same schema share a single request") {
    for {
      gate <- IO.deferred[Unit]
      recorded <- recording(_ => gate.get.as(respond(Status.Ok, schemaResponse)))
      client <- clientOf(recorded.app)()
      fiber <- client.getSchema(SchemaId(1)).parReplicateA(10).start
      _ <- IO.sleep(100.millis)
      _ <- gate.complete(())
      schemas <- fiber.joinWithNever
      requests <- recorded.requests
    } yield expect.eql(List.fill(10)(schema), schemas) && expect.eql(1, requests.size)
  }

  test("requests waiting for a canceled request send the request again") {
    for {
      count <- IO.ref(0)
      received <- IO.deferred[Unit]
      recorded <- recording { _ =>
        count.getAndUpdate(_ + 1).flatMap {
          case 0 => received.complete(()) >> IO.never
          case _ => IO.pure(respond(Status.Ok, schemaResponse))
        }
      }
      client <- clientOf(recorded.app)()
      first <- client.getSchema(SchemaId(1)).start
      _ <- received.get
      waiting <- client.getSchema(SchemaId(1)).start
      _ <- IO.sleep(100.millis)
      _ <- first.cancel
      found <- waiting.joinWithNever
      requests <- recorded.requests
    } yield expect.eql(schema, found) && expect.eql(2, requests.size)
  }

  test("canceled requests are not cached, so the next request sends the request again") {
    for {
      count <- IO.ref(0)
      received <- IO.deferred[Unit]
      recorded <- recording { _ =>
        count.getAndUpdate(_ + 1).flatMap {
          case 0 => received.complete(()) >> IO.never
          case _ => IO.pure(respond(Status.Ok, schemaResponse))
        }
      }
      client <- clientOf(recorded.app)()
      first <- client.getSchema(SchemaId(1)).start
      _ <- received.get
      _ <- first.cancel
      found <- client.getSchema(SchemaId(1))
      cached <- client.getSchema(SchemaId(1))
      requests <- recorded.requests
    } yield expect.eql(schema, found) && expect.eql(schema, cached) && expect.eql(2, requests.size)
  }

  test("canceling requests waiting for another request does not cancel the other request") {
    TestControl.executeEmbed {
      for {
        recorded <- recording(_ => IO.sleep(100.millis).as(respond(Status.Ok, schemaResponse)))
        client <- clientOf(recorded.app)()
        first <- client.getSchema(SchemaId(1)).start
        _ <- IO.sleep(10.millis)
        waiting <- client.getSchema(SchemaId(1)).start
        _ <- IO.sleep(10.millis)
        _ <- waiting.cancel
        found <- first.joinWithNever
        outcome <- waiting.join
        cached <- client.getSchema(SchemaId(1))
        requests <- recorded.requests
      } yield expect.eql(schema, found) &&
        expect(outcome.isCanceled) &&
        expect.eql(schema, cached) &&
        expect.eql(1, requests.size)
    }
  }

  test("requests waiting for a request which short-circuits in a monad transformer send the request again") {
    type F[A] = EitherT[IO, String, A]
    TestControl.executeEmbed {
      for {
        count <- IO.ref(0)
        send = EitherT.liftF[IO, String, Int](count.getAndUpdate(_ + 1)).flatMap {
          case 0 =>
            EitherT.liftF[IO, String, Unit](IO.sleep(100.millis)) >> EitherT.leftT[IO, Response[F]]("Left")
          case _ => EitherT.pure[IO, String](Response[F](Status.Ok).withEntity(schemaResponse))
        }
        built <- SchemaRegistryClientBuilder
          .default[F](Client[F](_ => Resource.eval(send)), uri"http://localhost:8081")
          .build
          .value
        client <- IO.fromEither(built.leftMap(new RuntimeException(_)))
        first <- client.getSchema(SchemaId(1)).value.start
        _ <- IO.sleep(10.millis)
        waiting <- client.getSchema(SchemaId(1)).value.start
        shortCircuited <- first.joinWithNever
        found <- waiting.joinWithNever
        cached <- client.getSchema(SchemaId(1)).value
        requests <- count.get
      } yield expect.eql("Left".asLeft[Schema], shortCircuited) &&
        expect.eql(schema.asRight[String], found) &&
        expect.eql(schema.asRight[String], cached) &&
        expect.eql(2, requests)
    }
  }

  test("concurrent requests share failures, which are not cached") {
    TestControl.executeEmbed {
      for {
        count <- IO.ref(0)
        recorded <- recording { _ =>
          count.getAndUpdate(_ + 1).flatMap {
            case 0 =>
              IO.sleep(100.millis)
                .as(respond(Status.InternalServerError, """{"error_code":50001,"message":"Error"}"""))
            case _ =>
              IO.pure(respond(Status.Ok, schemaResponse))
          }
        }
        client <- clientOf(recorded.app)()
        failed <- client.getSchema(SchemaId(1)).attempt.parReplicateA(10)
        found <- client.getSchema(SchemaId(1))
        requests <- recorded.requests
      } yield forEach(failed) { result =>
        matches(result) { case Left(error: SchemaRegistryError) =>
          expect.eql(Some(SchemaRegistryErrorCode(50001)), error.errorCode)
        }
      } && expect.eql(schema, found) && expect.eql(2, requests.size)
    }
  }

  test("latest versions expire when the expiry has passed since the response was received") {
    val subject = SubjectName("subject")
    TestControl.executeEmbed {
      for {
        recorded <- recording(_ => IO.sleep(100.millis).as(respond(Status.Ok, registeredResponse)))
        client <- clientOf(recorded.app)(_.withLatestCacheExpiry(50.millis))
        _ <- client.getSubjectVersion(subject, VersionSelector.Latest)
        _ <- IO.sleep(49.millis)
        _ <- client.getSubjectVersion(subject, VersionSelector.Latest)
        cached <- recorded.requests
        _ <- IO.sleep(1.millis)
        _ <- client.getSubjectVersion(subject, VersionSelector.Latest)
        requests <- recorded.requests
      } yield expect.eql(1, cached.size) && expect.eql(2, requests.size)
    }
  }

  test("latest versions are not cached for undefined expiries, and are cached for the longest expiry") {
    val subject = SubjectName("subject")
    TestControl.executeEmbed {
      for {
        recorded <- recording(_ => IO.pure(respond(Status.Ok, registeredResponse)))
        undefined <- clientOf(recorded.app)(_.withLatestCacheExpiry(Duration.Undefined))
        longest <- clientOf(recorded.app)(_.withLatestCacheExpiry(Long.MaxValue.nanos))
        _ <- IO.sleep(1.millis) // Should not cause overflow
        _ <- undefined.getSubjectVersion(subject, VersionSelector.Latest).replicateA_(2)
        uncached <- recorded.requests
        _ <- longest.getSubjectVersion(subject, VersionSelector.Latest).replicateA_(2)
        requests <- recorded.requests
      } yield expect.eql(2, uncached.size) && expect.eql(3, requests.size)
    }
  }

  test("concurrent requests for expired latest versions share a single request") {
    val subject = SubjectName("subject")
    TestControl.executeEmbed {
      for {
        recorded <- recording(_ => IO.sleep(10.millis).as(respond(Status.Ok, registeredResponse)))
        client <- clientOf(recorded.app)(_.withLatestCacheExpiry(50.millis))
        _ <- client.getSubjectVersion(subject, VersionSelector.Latest)
        _ <- IO.sleep(100.millis)
        versions <- client.getSubjectVersion(subject, VersionSelector.Latest).parReplicateA(10)
        requests <- recorded.requests
      } yield expect.eql(10, versions.size) && expect.eql(2, requests.size)
    }
  }

  test("concurrent lookups of schemas which are not registered share a single request, which is not cached") {
    val notFound = """{"error_code":40403,"message":"Schema not found"}"""
    TestControl.executeEmbed {
      for {
        recorded <- recording(_ => IO.sleep(10.millis).as(respond(Status.NotFound, notFound)))
        client <- clientOf(recorded.app)()
        subject = SubjectName("subject")
        missing <- client.lookupSchema(subject, schema).parReplicateA(10)
        _ <- client.lookupSchema(subject, schema)
        requests <- recorded.requests
      } yield expect.eql(List.fill(10)(Option.empty[RegisteredSchema]), missing) &&
        expect.eql(2, requests.size)
    }
  }

  test("registering schemas clears cached latest versions, unless the registration is cached") {
    for {
      recorded <- recording { request =>
        val body = if (request.method == Method.GET) registeredResponse else """{"id":1}"""
        IO.pure(respond(Status.Ok, body))
      }
      client <- clientOf(recorded.app)()
      subject = SubjectName("subject")
      latest = client.getSubjectVersion(subject, VersionSelector.Latest)
      _ <- latest >> latest
      _ <- client.registerSchema(subject, schema)
      _ <- latest >> latest
      _ <- client.registerSchema(subject, schema)
      _ <- client.registerSchema(SubjectName("other"), schema)
      _ <- latest
      requests <- recorded.requests
    } yield expect.eql(
      List(
        "GET /subjects/subject/versions/latest",
        "POST /subjects/subject/versions",
        "GET /subjects/subject/versions/latest",
        "POST /subjects/other/versions"
      ),
      requests.map(request => s"${request.method} ${request.uri.path.renderString}")
    )
  }

  test("registering schemas clears cached latest versions when the registration fails or is canceled") {
    val failing = SubjectName("failing")
    val canceled = SubjectName("canceled")
    for {
      received <- IO.deferred[Unit]
      recorded <- recording { request =>
        request.uri.path.renderString match {
          case "/subjects/failing/versions" => IO.pure(respond(Status.GatewayTimeout, "Gateway timeout"))
          case "/subjects/canceled/versions" => received.complete(()) >> IO.never
          case _ => IO.pure(respond(Status.Ok, registeredResponse))
        }
      }
      client <- clientOf(recorded.app)()
      latest = (subject: SubjectName) => client.getSubjectVersion(subject, VersionSelector.Latest)
      _ <- latest(failing)
      failed <- client.registerSchema(failing, schema).attempt
      _ <- latest(failing)
      _ <- latest(canceled)
      registering <- client.registerSchema(canceled, schema).start
      _ <- received.get
      _ <- registering.cancel
      outcome <- registering.join
      _ <- latest(canceled)
      requests <- recorded.requests
    } yield matches(failed) { case Left(error: SchemaRegistryError) =>
      expect.same(Status.GatewayTimeout, error.status)
    } && expect(outcome.isCanceled) &&
      expect.eql(
        List(
          "GET /subjects/failing/versions/latest",
          "POST /subjects/failing/versions",
          "GET /subjects/failing/versions/latest",
          "GET /subjects/canceled/versions/latest",
          "POST /subjects/canceled/versions",
          "GET /subjects/canceled/versions/latest"
        ),
        requests.map(request => s"${request.method} ${request.uri.path.renderString}")
      )
  }

  test("schemas included in responses are cached by id and GUID") {
    def guid(n: Int): SchemaGuid =
      SchemaGuid(s"00000000-0000-0000-0000-00000000000$n")

    def registered(subject: String, n: Int): String =
      s"""{"subject":"$subject","version":1,"id":$n,"schema":"\\"string\\"","guid":"${guid(n).value}"}"""

    for {
      recorded <- recording { request =>
        val body =
          request.uri.path.renderString match {
            case "/subjects/a/versions" =>
              s"""{"id":1,"version":1,"schema":"\\"string\\"","guid":"${guid(1).value}"}"""
            case "/subjects/b/versions" => """{"id":2}"""
            case "/subjects/c" => registered("c", 3)
            case "/subjects/d/versions/latest" => registered("d", 4)
            case _ => schemaResponse
          }
        IO.pure(respond(Status.Ok, body))
      }
      client <- clientOf(recorded.app)()
      _ <- client.registerSchema(SubjectName("a"), schema)
      _ <- client.registerSchema(SubjectName("b"), schema)
      _ <- client.lookupSchema(SubjectName("c"), schema)
      _ <- client.getSubjectVersion(SubjectName("d"), VersionSelector.Latest)
      byId <- List(1, 2, 3, 4).traverse(id => client.getSchema(SchemaId(id)))
      byGuid <- List(1, 3, 4).traverse(n => client.getSchemaByGuid(guid(n)))
      requests <- recorded.requests
    } yield expect.eql(List.fill(4)(schema), byId) &&
      expect.eql(List.fill(3)(schema), byGuid) &&
      expect.eql(
        List(
          "/subjects/a/versions",
          "/subjects/b/versions",
          "/subjects/c",
          "/subjects/d/versions/latest",
          "/schemas/ids/2"
        ),
        requests.map(_.uri.path.renderString)
      )
  }

  test("caching does not change the results of requests") {
    case class Operation(name: String, run: SchemaRegistryClient[IO] => IO[Any]) {
      override def toString: String = name
    }

    implicit val operationShow: Show[Operation] = Show.fromToString

    val subjects = Gen.oneOf(SubjectName("a"), SubjectName("b"))
    val ids = Gen.choose(1, 4).map(SchemaId(_))
    val guids = ids.map(id => SchemaGuid(new UUID(0L, id.value.toLong).toString))
    val versions = Gen.oneOf(
      VersionSelector.Latest :: List(-1, 0, 1, 2, 3).map(SubjectVersion(_)).map(VersionSelector.Exact)
    )
    val schemas =
      Gen.oneOf(
        Schema("\"string\""),
        Schema(" \"string\" "),
        Schema("\"int\""),
        Schema("\"long\""),
        Schema("\"boolean\""),
        Schema("syntax = \"proto3\";").withSchemaType(SchemaType.Protobuf),
        Schema("""{"type":""")
      )

    val operations =
      Gen.oneOf(
        ids.map(id => Operation(s"getSchema($id)", _.getSchema(id))),
        guids.map(guid => Operation(s"getSchemaByGuid($guid)", _.getSchemaByGuid(guid))),
        Gen.zip(subjects, schemas).map { case (subject, schema) =>
          Operation(s"registerSchema($subject, $schema)", _.registerSchema(subject, schema))
        },
        Gen.zip(subjects, schemas).map { case (subject, schema) =>
          Operation(s"lookupSchema($subject, $schema)", _.lookupSchema(subject, schema))
        },
        Gen.zip(subjects, versions).map { case (subject, version) =>
          Operation(s"getSubjectVersion($subject, $version)", _.getSubjectVersion(subject, version))
        },
        Gen.zip(subjects, schemas, versions).map { case (subject, schema, version) =>
          Operation(
            s"checkCompatibility($subject, $schema, $version)",
            _.checkCompatibility(subject, schema, version)
          )
        }
      )

    val compatibility: (SubjectName, Schema, List[RegisteredSchema]) => Compatibility =
      (_, schema, versions) =>
        if (versions.forall(_.schema.schemaType == schema.schemaType)) Compatibility.Compatible
        else Compatibility.Incompatible(List("Schema type differs from earlier versions"))

    def resultsOf(operations: List[Operation], caching: Boolean): IO[List[Either[Throwable, Any]]] =
      for {
        registry <- TestSchemaRegistry.withCompatibility[IO](compatibility)
        client <- registry.client.withCaching(caching).build
        results <- operations.traverse(_.run(client).attempt)
      } yield results

    forall(Gen.choose(0, 60).flatMap(Gen.listOfN(_, operations))) { operations =>
      val repeated = operations ++ operations // Repeat to find outdated cached results after registrations
      (resultsOf(repeated, caching = false), resultsOf(repeated, caching = true)).mapN(expect.same(_, _))
    }
  }

  test("auth and builder hide secrets in toString") {
    val credentials =
      Gen.zip(stringsWithSpecialCharacters, stringsWithSpecialCharacters, stringsWithSpecialCharacters)

    forall(credentials) { case (username, password, token) =>
      val builder =
        SchemaRegistryClientBuilder
          .default(Client.fromHttpApp(HttpApp.notFound[IO]), uri"http://localhost:8081")
          .withAuth(Auth.Basic(username, password))

      expect.eql(s"Basic($username, ***)", Auth.Basic(username, password).toString) &&
      expect.eql("Bearer(***)", Auth.Bearer(token).toString) &&
      expect.eql("None", Auth.None.toString) &&
      expect.eql(
        s"SchemaRegistryClientBuilder(uri = http://localhost:8081, auth = Basic($username, ***), caching = true, latestCacheExpiry = Duration.Inf)",
        builder.toString
      )
    }
  }
}
