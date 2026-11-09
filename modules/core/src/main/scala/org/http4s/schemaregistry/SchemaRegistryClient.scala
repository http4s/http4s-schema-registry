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

import cats.Applicative
import cats.effect.Concurrent
import cats.effect.Deferred
import cats.effect.Outcome
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.MapRef
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.jawn
import io.circe.syntax.*
import org.http4s.Header
import org.http4s.Headers
import org.http4s.InvalidMessageBodyFailure
import org.http4s.MediaType
import org.http4s.Method
import org.http4s.ParseFailure
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.headers.Accept
import org.http4s.headers.`Content-Type`
import org.http4s.syntax.all.*
import org.typelevel.ci.CIString
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

/**
  * Client for a schema registry implementing the Confluent Schema Registry REST API.
  *
  * Use [[SchemaRegistryClientBuilder]] to create a client, or [[TestSchemaRegistry]]
  * for a client of an in-memory registry for tests. Errors from the registry will be
  * raised as [[SchemaRegistryError]]s.
  */
sealed abstract class SchemaRegistryClient[F[_]] {

  /** Returns the schema with the specified id, raising an error if it does not exist. */
  def getSchema(id: SchemaId): F[Schema]

  /** Returns the schema with the specified id, or `None` if it does not exist. */
  def getSchemaOption(id: SchemaId): F[Option[Schema]]

  /** Returns the schema with the specified GUID, raising an error if it does not exist. */
  def getSchemaByGuid(guid: SchemaGuid): F[Schema]

  /** Returns the schema with the specified GUID, or `None` if it does not exist. */
  def getSchemaByGuidOption(guid: SchemaGuid): F[Option[Schema]]

  /**
    * Registers the schema, unless already registered, and returns its [[SchemaId]].
    *
    * If `normalize` is `true`, the registry normalizes the schema before registering
    * it, so that semantically equivalent schemas are registered as the same schema.
    */
  def registerSchema(subject: SubjectName, schema: Schema, normalize: Boolean): F[SchemaId]

  /** Registers the schema, unless already registered, and returns its [[SchemaId]]. */
  final def registerSchema(subject: SubjectName, schema: Schema): F[SchemaId] =
    registerSchema(subject, schema, normalize = false)

  /**
    * Returns the schema as registered under the subject, or `None` if
    * the subject does not exist, or if the schema is not registered.
    *
    * If `normalize` is `true`, the registry normalizes the schema
    * before looking it up.
    */
  def lookupSchema(subject: SubjectName, schema: Schema, normalize: Boolean): F[Option[RegisteredSchema]]

  /**
    * Returns the schema as registered under the subject, or `None` if
    * the subject does not exist, or if the schema is not registered.
    */
  final def lookupSchema(subject: SubjectName, schema: Schema): F[Option[RegisteredSchema]] =
    lookupSchema(subject, schema, normalize = false)

  /** Returns the specified version of the subject, raising an error if it does not exist. */
  def getSubjectVersion(subject: SubjectName, version: VersionSelector): F[RegisteredSchema]

  /** Returns the specified version of the subject, or `None` if it does not exist. */
  def getSubjectVersionOption(subject: SubjectName, version: VersionSelector): F[Option[RegisteredSchema]]

  /**
    * Checks whether the schema is compatible with the specified version of the subject,
    * according to the compatibility level of the subject. Raises an error in case the
    * version does not exist.
    */
  def checkCompatibility(subject: SubjectName, schema: Schema, version: VersionSelector): F[Compatibility]

  /**
    * Checks whether the schema is compatible with the latest version of the subject,
    * according to the compatibility level of the subject. Raises an error in case
    * the version does not exist, but some registries instead return compatible
    * for subjects without versions.
    */
  final def checkCompatibility(subject: SubjectName, schema: Schema): F[Compatibility] =
    checkCompatibility(subject, schema, VersionSelector.Latest)
}

object SchemaRegistryClient {
  private[schemaregistry] def fromBuilder[F[_]](
    builder: SchemaRegistryClientBuilder[F]
  )(implicit F: Temporal[F]): F[SchemaRegistryClient[F]] = {
    val caches =
      if (builder.caching) Caches.memoize[F](builder.latestCacheExpiry)
      else Caches.none[F].pure[F]

    caches.map(new SchemaRegistryClientImpl[F](builder.client, builder.uri, builder.auth, _))
  }

  private final class SchemaRegistryClientImpl[F[_]](
    client: Client[F],
    uri: Uri,
    auth: Auth,
    caches: Caches[F]
  )(implicit F: Concurrent[F])
    extends SchemaRegistryClient[F] {

    private val contentType: `Content-Type` =
      `Content-Type`(mediaType"application/vnd.schemaregistry.v1+json")

    private val headers: Headers =
      Headers(
        Accept(
          mediaType"application/vnd.schemaregistry.v1+json",
          mediaType"application/vnd.schemaregistry+json".withQValue(qValue"0.9"),
          MediaType.application.json.withQValue(qValue"0.5")
        ),
        `Confluent-Accept-Unknown-Properties`(true),
        auth.authorization
      )

    override def getSchema(id: SchemaId): F[Schema] =
      caches.schemasById(id) {
        get(uri / "schemas" / "ids" / id.value.toString, Decoder[Schema])
      }

    override def getSchemaOption(id: SchemaId): F[Option[Schema]] =
      notFoundAsNone(SchemaRegistryErrorCode.SchemaNotFound)(getSchema(id))

    override def getSchemaByGuid(guid: SchemaGuid): F[Schema] =
      caches.schemasByGuid(guid) {
        get(uri / "schemas" / "guids" / guid.value, Decoder[Schema])
      }

    override def getSchemaByGuidOption(guid: SchemaGuid): F[Option[Schema]] =
      notFoundAsNone(SchemaRegistryErrorCode.SchemaNotFound)(getSchemaByGuid(guid))

    override def registerSchema(subject: SubjectName, schema: Schema, normalize: Boolean): F[SchemaId] =
      caches.registrations(SchemaKey(subject, schema, normalize)) {
        post(
          withNormalize(uri / "subjects" / subject.value / "versions", normalize),
          schema,
          registrationDecoder
        )
          .flatTap { case (id, registered, guid) => registered.traverseVoid(cacheSchema(id, _, guid)) }
          .map { case (id, _, _) => id }
          .guarantee(caches.latestVersions.invalidate(subject))
      }

    override def lookupSchema(
      subject: SubjectName,
      schema: Schema,
      normalize: Boolean
    ): F[Option[RegisteredSchema]] =
      caches.lookups(SchemaKey(subject, schema, normalize)) {
        notFoundAsNone(SchemaRegistryErrorCode.SubjectNotFound, SchemaRegistryErrorCode.SchemaNotFound) {
          post(withNormalize(uri / "subjects" / subject.value, normalize), schema, Decoder[RegisteredSchema])
        }.flatTap(_.traverseVoid(cacheRegisteredSchema))
      }

    override def getSubjectVersion(subject: SubjectName, version: VersionSelector): F[RegisteredSchema] = {
      def fetch: F[RegisteredSchema] =
        get(
          uri / "subjects" / subject.value / "versions" / versionSegment(version),
          Decoder[RegisteredSchema]
        ).flatTap(cacheRegisteredSchema)

      version.exactVersion match {
        case None => caches.latestVersions(subject)(fetch)
        case Some(exact) if exact.value >= 1 => caches.exactVersions(VersionKey(subject, version))(fetch)
        case Some(_) => fetch
      }
    }

    override def getSubjectVersionOption(
      subject: SubjectName,
      version: VersionSelector
    ): F[Option[RegisteredSchema]] =
      notFoundAsNone(SchemaRegistryErrorCode.SubjectNotFound, SchemaRegistryErrorCode.VersionNotFound) {
        getSubjectVersion(subject, version)
      }

    override def checkCompatibility(
      subject: SubjectName,
      schema: Schema,
      version: VersionSelector
    ): F[Compatibility] =
      post(
        (uri / "compatibility" / "subjects" / subject.value / "versions" / versionSegment(version))
          .withQueryParam("verbose", "true"),
        schema,
        Decoder[Compatibility]
      )

    override def toString: String =
      s"SchemaRegistryClient($uri)"

    private def get[A](uri: Uri, decoder: Decoder[A]): F[A] =
      run(Request[F](Method.GET, uri), decoder)

    private def post[A](uri: Uri, schema: Schema, decoder: Decoder[A]): F[A] =
      run(
        Request[F](Method.POST, uri).withEntity(schema.asJson.noSpaces).withContentType(contentType),
        decoder
      )

    private def run[A](request: Request[F], decoder: Decoder[A]): F[A] =
      client.run(request.putHeaders(headers)).use { response =>
        response.as[String].flatMap { body =>
          if (response.status.responseClass == Status.Successful)
            jawn.decode(body)(decoder) match {
              case Right(a) =>
                F.pure(a)
              case Left(error) =>
                F.raiseError(
                  InvalidMessageBodyFailure(
                    s"Unexpected response from schema registry for ${request.method} ${request.uri.path}",
                    Some(error)
                  )
                )
            }
          else F.raiseError(SchemaRegistryClient.error(response.status, body))
        }
      }

    private def notFoundAsNone[A](errorCodes: SchemaRegistryErrorCode*)(fa: F[A]): F[Option[A]] =
      fa.map(_.some).recover {
        case error: SchemaRegistryError
          if error.status == Status.NotFound && error.errorCode.exists(errorCodes.contains) =>
          None
      }

    private def cacheSchema(id: SchemaId, schema: Schema, guid: Option[SchemaGuid]): F[Unit] =
      caches.schemasById.put(id, schema) >> guid.traverseVoid(caches.schemasByGuid.put(_, schema))

    private def cacheRegisteredSchema(registered: RegisteredSchema): F[Unit] =
      cacheSchema(registered.id, registered.schema, registered.guid)

    private def withNormalize(uri: Uri, normalize: Boolean): Uri =
      if (normalize) uri.withQueryParam("normalize", "true") else uri

    private def versionSegment(version: VersionSelector): String =
      version.exactVersion.fold("latest")(_.value.toString)
  }

  /** Header for whether Confluent Schema Registry includes extended properties (e.g. GUIDs) in responses. */
  private final case class `Confluent-Accept-Unknown-Properties`(value: Boolean)

  private object `Confluent-Accept-Unknown-Properties` {
    implicit val header: Header[`Confluent-Accept-Unknown-Properties`, Header.Single] =
      Header.create(
        CIString("Confluent-Accept-Unknown-Properties"),
        _.value.toString,
        value =>
          value.toBooleanOption
            .map(`Confluent-Accept-Unknown-Properties`(_))
            .toRight(ParseFailure("Invalid Confluent-Accept-Unknown-Properties header", value))
      )
  }

  /** Key for caching results of registering and looking up schemas. */
  private final case class SchemaKey(subject: SubjectName, schema: Schema, normalize: Boolean)

  /** Key for caching exact versions of subjects. */
  private final case class VersionKey(subject: SubjectName, version: VersionSelector)

  /** The caches of a client, where the latest versions of subjects can expire. */
  private final case class Caches[F[_]](
    schemasById: Cache[F, SchemaId, Schema],
    schemasByGuid: Cache[F, SchemaGuid, Schema],
    registrations: Cache[F, SchemaKey, SchemaId],
    lookups: Cache[F, SchemaKey, Option[RegisteredSchema]],
    exactVersions: Cache[F, VersionKey, RegisteredSchema],
    latestVersions: Cache[F, SubjectName, RegisteredSchema]
  )

  private object Caches {

    /** Returns a new [[Caches]] which does not cache anything. */
    def none[F[_]: Applicative]: Caches[F] =
      Caches(Cache.none, Cache.none, Cache.none, Cache.none, Cache.none, Cache.none)

    /**
      * Returns a new [[Caches]] with infinite caching for schemas.
      * The exception is latest versions, which are cached using an
      * expiry time and are invalidated on schema registration.
      */
    def memoize[F[_]](latestExpiry: Duration)(implicit F: Temporal[F]): F[Caches[F]] =
      (
        Cache.memoize[F, SchemaId, Schema](_ => true, None),
        Cache.memoize[F, SchemaGuid, Schema](_ => true, None),
        Cache.memoize[F, SchemaKey, SchemaId](_ => true, None),
        Cache.memoize[F, SchemaKey, Option[RegisteredSchema]](_.isDefined, None),
        Cache.memoize[F, VersionKey, RegisteredSchema](_ => true, None),
        Cache.memoize[F, SubjectName, RegisteredSchema](_ => true, finiteExpiry(latestExpiry))
      ).mapN(apply)

    private def finiteExpiry(expiry: Duration): Option[FiniteDuration] =
      expiry match {
        case finite: FiniteDuration => Some(finite)
        case Duration.Inf => None
        case _ => Some(Duration.Zero)
      }
  }

  /** Cache of values by key, fetching values which are not cached. */
  private sealed abstract class Cache[F[_], K, V] {

    /**
      * Returns the value for the key, fetching it if it is not cached.
      *
      * The fetch is only evaluated when fetching, so that cached values
      * are returned without building requests.
      */
    def apply(key: K)(fetch: => F[V]): F[V]

    /**
      * Caches the value for the key, with any expiry counted from now, replacing
      * cached values which expire. Values being fetched, and values which do not
      * expire, are kept instead.
      */
    def put(key: K, value: V): F[Unit]

    /** Removes the value for the key, so that the value is fetched again. */
    def invalidate(key: K): F[Unit]
  }

  private object Cache {

    /** Returns a cache which does not cache, and fetches values every time. */
    def none[F[_], K, V](implicit F: Applicative[F]): Cache[F, K, V] =
      new Cache[F, K, V] {
        override def apply(key: K)(fetch: => F[V]): F[V] =
          fetch

        override def put(key: K, value: V): F[Unit] =
          F.unit

        override def invalidate(key: K): F[Unit] =
          F.unit
      }

    /**
      * Returns a cache which keeps fetched values for which `keep`
      * returns `true`, until the expiry has passed since fetching.
      *
      * Concurrent requests for a key share a single fetch. This
      * also includes failures, but failures are not kept. Whenever
      * a fetch is canceled, or does not complete, waiting requests
      * fetch again instead of being canceled.
      */
    def memoize[F[_]: Temporal, K, V](keep: V => Boolean, expiry: Option[FiniteDuration]): F[Cache[F, K, V]] =
      MapRef[F, K, Entry[F, V]].map(new MemoizeCache(_, keep, expiry))

    /**
      * Entry with the result of a fetch, and when the value was fetched if it expires,
      * which is `None` while the value is being fetched. Values expire by comparing the
      * time since they were fetched with the expiry, which cannot overflow, unlike the
      * time when they expire.
      */
    private final case class Entry[F[_], V](
      result: Deferred[F, Option[Either[Throwable, V]]],
      fetchedAt: Option[FiniteDuration]
    )

    private final class MemoizeCache[F[_], K, V](
      entries: MapRef[F, K, Option[Entry[F, V]]],
      keep: V => Boolean,
      expiry: Option[FiniteDuration]
    )(implicit F: Temporal[F])
      extends Cache[F, K, V] {
      override def apply(key: K)(fetch: => F[V]): F[V] =
        getOrFetch(key, F.unit.flatMap(_ => fetch))

      private def getOrFetch(key: K, fetch: F[V]): F[V] = {
        val entry = entries(key)
        now.flatMap { now =>
          entry.get.flatMap {
            case Some(existing) if valid(existing, now) =>
              await(key, fetch, existing.result)

            case _ =>
              F.deferred[Option[Either[Throwable, V]]].flatMap { result =>
                entry.flatModifyFull {
                  case (poll, Some(existing)) if valid(existing, now) =>
                    (Some(existing), poll(await(key, fetch, existing.result)))
                  case (poll, _) =>
                    (Some(Entry(result, None)), run(entry, result, poll(fetch)))
                }
              }
          }
        }
      }

      override def put(key: K, value: V): F[Unit] =
        if (keep(value))
          (now, F.deferred[Option[Either[Throwable, V]]]).flatMapN { (now, result) =>
            result.complete(Some(Right(value))) >> entries(key).update {
              case Some(existing) if existing.fetchedAt.isEmpty => Some(existing)
              case _ => Some(Entry(result, expiry.as(now)))
            }
          }
        else F.unit

      override def invalidate(key: K): F[Unit] =
        entries(key).set(None)

      private def now: F[FiniteDuration] =
        if (expiry.isDefined) F.monotonic else F.pure(Duration.Zero)

      private def valid(entry: Entry[F, V], now: FiniteDuration): Boolean =
        entry.fetchedAt.forall(fetchedAt => expiry.forall(now - fetchedAt < _))

      private def await(key: K, fetch: F[V], result: Deferred[F, Option[Either[Throwable, V]]]): F[V] =
        result.get.flatMap {
          case Some(fetched) => fetched.liftTo[F]
          case None => getOrFetch(key, fetch)
        }

      private def run(
        entry: Ref[F, Option[Entry[F, V]]],
        result: Deferred[F, Option[Either[Throwable, V]]],
        fetch: F[V]
      ): F[V] =
        fetch
          .flatTap { value =>
            val update = if (keep(value)) refresh(entry, result) else remove(entry, result)
            update >> result.complete(Some(Right(value))).void
          }
          .guaranteeCase {
            case Outcome.Errored(e) =>
              remove(entry, result) >> result.complete(Some(Left(e))).void
            case _ =>
              result.tryGet.flatMap {
                case Some(_) => F.unit
                case None => remove(entry, result) >> result.complete(None).void
              }
          }

      private def refresh(
        entry: Ref[F, Option[Entry[F, V]]],
        result: Deferred[F, Option[Either[Throwable, V]]]
      ): F[Unit] =
        if (expiry.isDefined)
          F.monotonic.flatMap { now =>
            entry.update {
              case Some(current) if current.result eq result =>
                Some(current.copy(fetchedAt = Some(now)))
              case other =>
                other
            }
          }
        else F.unit

      private def remove(
        entry: Ref[F, Option[Entry[F, V]]],
        result: Deferred[F, Option[Either[Throwable, V]]]
      ): F[Unit] =
        entry.update {
          case Some(current) if current.result eq result => None
          case other => other
        }
    }
  }

  private val registrationDecoder: Decoder[(SchemaId, Option[Schema], Option[SchemaGuid])] =
    Decoder.instance { c =>
      for {
        id <- c.get[SchemaId]("id")
        hasSchema = c.downField("schema").focus.exists(!_.isNull)
        schema <- if (hasSchema) c.as[Schema].map(Some(_)) else Right(None)
        guid <- c.get[Option[SchemaGuid]]("guid")
      } yield (id, schema, guid)
    }

  private def error(status: Status, body: String): SchemaRegistryError =
    jawn.decode(body)(errorDecoder) match {
      case Right((errorCode, message)) =>
        SchemaRegistryError(status, Some(errorCode), message)
      case Left(_) =>
        SchemaRegistryError(status, None, body.trim)
    }

  private val errorDecoder: Decoder[(SchemaRegistryErrorCode, String)] =
    Decoder.instance { c =>
      for {
        errorCode <- c.get[SchemaRegistryErrorCode]("error_code")
        message <- c.getOrElse[String]("message")("")
      } yield (errorCode, message)
    }
}
