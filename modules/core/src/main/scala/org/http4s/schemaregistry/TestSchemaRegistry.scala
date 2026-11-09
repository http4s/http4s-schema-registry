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

import cats.data.Kleisli
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.jawn
import io.circe.syntax.*
import java.util.UUID
import org.http4s.HttpApp
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import org.http4s.syntax.all.*

/**
  * An in-memory schema registry for testing purposes.
  *
  * Schemas are the same if they have the same definition, schema type, and references.
  * Definitions are compared as JSON, ignoring formatting. A schema has the same id in
  * all subjects it is registered. Whether a schema is compatible with versions of a
  * subject is decided by the provided function when creating the registry.
  *
  * Use [[TestSchemaRegistry.apply]] for a registry where all schemas are compatible,
  * or [[TestSchemaRegistry.withCompatibility]] for custom compatibility decisions.
  */
sealed abstract class TestSchemaRegistry[F[_]] {

  /** The REST API of the registry, for example for use with `Client.fromHttpApp`. */
  def httpApp: HttpApp[F]

  /** Returns a builder for a client of the registry, with default options. */
  def client: SchemaRegistryClientBuilder[F]

  /** Returns all subject versions, in the order they were registered. */
  def registered: F[List[RegisteredSchema]]
}

object TestSchemaRegistry {

  /** Returns an empty registry where all schemas are compatible. */
  def apply[F[_]: Temporal]: F[TestSchemaRegistry[F]] =
    withCompatibility((_, _, _) => Compatibility.Compatible)

  /**
    * Returns an empty registry where `compatibility` decides whether
    * a schema is compatible with a subject version.
    *
    * When registering schemas, the registered schemas are all subject
    * versions. When checking compatibility, the registered schemas are
    * only the version for which compatibility is being checked.
    */
  def withCompatibility[F[_]](
    compatibility: (SubjectName, Schema, List[RegisteredSchema]) => Compatibility
  )(implicit F: Temporal[F]): F[TestSchemaRegistry[F]] =
    F.ref(State.empty).map(new TestSchemaRegistryImpl(_, compatibility))

  private final class TestSchemaRegistryImpl[F[_]](
    state: Ref[F, State],
    compatibility: (SubjectName, Schema, List[RegisteredSchema]) => Compatibility
  )(implicit F: Temporal[F])
    extends TestSchemaRegistry[F] {
    override val httpApp: HttpApp[F] =
      Kleisli { (request: Request[F]) =>
        (request.method, request.uri.path.segments.map(_.decoded()).toList) match {
          case (Method.GET, List("schemas", "ids", id)) =>
            getSchemaById(id)
          case (Method.GET, List("schemas", "guids", guid)) =>
            getSchemaByGuid(guid)
          case (Method.POST, List("subjects", subject, "versions")) =>
            request.as[String].flatMap(registerSchema(SubjectName(subject), _))
          case (Method.POST, List("subjects", subject)) =>
            request.as[String].flatMap(lookupSchema(SubjectName(subject), _))
          case (Method.GET, List("subjects", subject, "versions", version)) =>
            getSubjectVersion(SubjectName(subject), version)
          case (Method.POST, List("compatibility", "subjects", subject, "versions", version)) =>
            val verbose = request.params.get("verbose").contains("true")
            request.as[String].flatMap(checkCompatibility(SubjectName(subject), version, verbose, _))
          case _ =>
            F.pure(
              error(Status.NotFound, SchemaRegistryErrorCode(Status.NotFound.code), "HTTP 404 Not Found")
            )
        }
      }

    override def client: SchemaRegistryClientBuilder[F] =
      SchemaRegistryClientBuilder.default(Client(httpApp(_).toResource), Uri())

    override def registered: F[List[RegisteredSchema]] =
      state.get.map(_.versions.toList)

    override def toString: String =
      "TestSchemaRegistry"

    private def getSchemaById(id: String): F[Response[F]] =
      state.get.map { current =>
        id.toIntOption.map(SchemaId(_)).flatMap(id => current.schema(id).tupleRight(id)) match {
          case Some((schema, id)) => ok(schemaWithGuid(schema, guidOf(id)))
          case None => error(Status.NotFound, SchemaRegistryErrorCode.SchemaNotFound, s"Schema $id not found")
        }
      }

    private def getSchemaByGuid(guid: String): F[Response[F]] =
      state.get.map { current =>
        idOfGuid(guid).flatMap(id => current.schema(id).tupleRight(id)) match {
          case Some((schema, id)) => ok(schemaWithGuid(schema, guidOf(id)))
          case None =>
            error(Status.NotFound, SchemaRegistryErrorCode.SchemaNotFound, s"Schema $guid not found")
        }
      }

    private def registerSchema(subject: SubjectName, body: String): F[Response[F]] =
      decodeSchema(body) match {
        case Right(schema) =>
          state.modify { current =>
            val versions = current.versionsOf(subject)
            versions.find(version => same(version.schema, schema)) match {
              case Some(existing) =>
                (current, ok(registration(existing)))

              case None =>
                val result = compatibility(subject, schema, versions.toList)
                if (result.isCompatible) {
                  val (schemas, id) =
                    current.idOf(schema) match {
                      case Some(id) => (current.schemas, id)
                      case None => (current.schemas :+ schema, SchemaId(current.schemas.size + 1))
                    }

                  val version =
                    SubjectVersion(versions.lastOption.fold(1)(_.version.value + 1))

                  val registered =
                    RegisteredSchema(subject, id, version, schema).withGuid(guidOf(id))

                  (State(schemas, current.versions :+ registered), ok(registration(registered)))
                } else {
                  val details = result.errors.mkString("; ")
                  val message =
                    s"""Schema being registered is incompatible with an earlier schema for subject "${subject.value}", details: $details"""
                  (current, error(Status.Conflict, SchemaRegistryErrorCode.IncompatibleSchema, message))
                }
            }
          }

        case Left(response) =>
          F.pure(response)
      }

    private def lookupSchema(subject: SubjectName, body: String): F[Response[F]] =
      decodeSchema(body) match {
        case Right(schema) =>
          state.get.map { current =>
            val versions = current.versionsOf(subject)
            if (versions.isEmpty) subjectNotFound(subject)
            else
              versions.find(version => same(version.schema, schema)) match {
                case Some(registered) => ok(registered)
                case None =>
                  error(Status.NotFound, SchemaRegistryErrorCode.SchemaNotFound, "Schema not found")
              }
          }

        case Left(response) =>
          F.pure(response)
      }

    private def getSubjectVersion(subject: SubjectName, version: String): F[Response[F]] =
      parseVersion(version) match {
        case Some(selector) =>
          state.get.map { current =>
            val versions = current.versionsOf(subject)
            if (versions.isEmpty) subjectNotFound(subject)
            else
              select(versions, selector) match {
                case Some(registered) => ok(registered)
                case None => versionNotFound(version)
              }
          }

        case None =>
          F.pure(invalidVersion(version))
      }

    private def checkCompatibility(
      subject: SubjectName,
      version: String,
      verbose: Boolean,
      body: String
    ): F[Response[F]] =
      (decodeSchema(body), parseVersion(version).toRight(invalidVersion(version))).tupled match {
        case Right((schema, selector)) =>
          state.get.map { current =>
            val versions = current.versionsOf(subject)
            select(versions, selector) match {
              case Some(registered) =>
                ok(compatibilityResult(compatibility(subject, schema, List(registered)), verbose))
              case None if versions.isEmpty && selector.exactVersion.isEmpty =>
                ok(compatibilityResult(Compatibility.Compatible, verbose))
              case None =>
                versionNotFound(version)
            }
          }

        case Left(response) =>
          F.pure(response)
      }

    private def decodeSchema(body: String): Either[Response[F], Schema] =
      jawn.decode[Schema](body) match {
        case Right(schema) if isJsonType(schema.schemaType) && jawn.parse(schema.definition).isLeft =>
          Left(error(Status.UnprocessableContent, SchemaRegistryErrorCode.InvalidSchema, "Invalid schema"))
        case Right(schema) =>
          Right(schema)
        case Left(failure) =>
          Left(
            error(
              Status.UnprocessableContent,
              SchemaRegistryErrorCode(Status.UnprocessableContent.code),
              failure.getMessage
            )
          )
      }

    private def ok[A: Encoder](a: A): Response[F] =
      response(Status.Ok, a.asJson)

    private def error(status: Status, errorCode: SchemaRegistryErrorCode, message: String): Response[F] =
      response(status, errorJson(errorCode, message))

    private def errorJson(errorCode: SchemaRegistryErrorCode, message: String): Json =
      Json.obj(
        "error_code" -> errorCode.asJson,
        "message" -> Json.fromString(message)
      )

    private def response(status: Status, json: Json): Response[F] =
      Response[F](status).withEntity(json.noSpaces).withContentType(contentType)

    private def subjectNotFound(subject: SubjectName): Response[F] =
      error(
        Status.NotFound,
        SchemaRegistryErrorCode.SubjectNotFound,
        s"Subject '${subject.value}' not found."
      )

    private def versionNotFound(version: String): Response[F] =
      error(Status.NotFound, SchemaRegistryErrorCode.VersionNotFound, s"Version $version not found.")

    private def invalidVersion(version: String): Response[F] =
      error(
        Status.UnprocessableContent,
        SchemaRegistryErrorCode.InvalidVersion,
        s"""The specified version '$version' is not a valid version id. Allowed values are between [1, 2^31-1] and the string "latest""""
      )
  }

  private final case class State(schemas: Vector[Schema], versions: Vector[RegisteredSchema]) {
    def schema(id: SchemaId): Option[Schema] =
      schemas.lift(id.value - 1)

    def idOf(schema: Schema): Option[SchemaId] =
      schemas.indexWhere(same(_, schema)) match {
        case -1 => None
        case index => Some(SchemaId(index + 1))
      }

    def versionsOf(subject: SubjectName): Vector[RegisteredSchema] =
      versions.filter(_.subject == subject)
  }

  private object State {
    val empty: State =
      State(Vector.empty, Vector.empty)
  }

  private def schemaWithGuid(schema: Schema, guid: SchemaGuid): JsonObject =
    schema.asJsonObject.add("guid", guid.asJson)

  private def registration(registered: RegisteredSchema): JsonObject =
    registered.asJsonObject.remove("subject")

  private def compatibilityResult(compatibility: Compatibility, verbose: Boolean): JsonObject =
    if (verbose) compatibility.asJsonObject else compatibility.asJsonObject.remove("messages")

  private def same(first: Schema, second: Schema): Boolean =
    first.schemaType == second.schemaType &&
      first.references == second.references &&
      ((jawn.parse(first.definition), jawn.parse(second.definition)) match {
        case (Right(first), Right(second)) => first == second
        case _ => first.definition == second.definition
      })

  private def isJsonType(schemaType: SchemaType): Boolean =
    schemaType == SchemaType.Avro || schemaType == SchemaType.Json

  private def select(
    versions: Vector[RegisteredSchema],
    selector: VersionSelector
  ): Option[RegisteredSchema] =
    selector.exactVersion.fold(versions.lastOption)(version => versions.find(_.version == version))

  private def parseVersion(version: String): Option[VersionSelector] =
    version match {
      case "latest" | "-1" => Some(VersionSelector.Latest)
      case _ => version.toIntOption.filter(_ > 0).map(n => VersionSelector.Exact(SubjectVersion(n)))
    }

  private def guidOf(id: SchemaId): SchemaGuid =
    SchemaGuid(new UUID(0L, id.value.toLong).toString)

  private def idOfGuid(guid: String): Option[SchemaId] =
    Either.catchNonFatal(UUID.fromString(guid)).toOption.collect {
      case uuid
        if uuid.getMostSignificantBits == 0L &&
          uuid.getLeastSignificantBits > 0L &&
          uuid.getLeastSignificantBits <= Int.MaxValue =>
        SchemaId(uuid.getLeastSignificantBits.toInt)
    }

  private val contentType: `Content-Type` =
    `Content-Type`(mediaType"application/vnd.schemaregistry.v1+json")
}
