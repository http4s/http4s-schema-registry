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

import cats.effect.Temporal
import org.http4s.Uri
import org.http4s.client.Client
import scala.concurrent.duration.Duration

/**
  * Builder for a [[SchemaRegistryClient]] which sends requests using a `Client`.
  *
  * Use [[SchemaRegistryClientBuilder.default]] to create a new builder instance.
  */
sealed abstract class SchemaRegistryClientBuilder[F[_]] {

  /** The `Client` for sending requests to the schema registry. */
  def client: Client[F]

  /** The base `Uri` of the schema registry to be used. */
  def uri: Uri

  /** The authentication for requests. Default is [[Auth.None]]. */
  def auth: Auth

  /**
    * Returns whether schemas should be cached or not.
    *
    * Schemas are cached by id and GUID, versions of subjects, and from the
    * results of registering and looking up schemas. Errors are not cached.
    * Latest subject versions are cached based on [[latestCacheExpiry]], by
    * default caching indefinitely, or until a new version of the subject
    * is registered using the client.
    *
    * Caching is enabled by default.
    */
  def caching: Boolean

  /**
    * Returns for how long the latest subject versions are cached.
    *
    * This only applies when [[caching]] is enabled. Latest versions
    * are cached indefinitely for `Duration.Inf`, and are not cached
    * for durations which are zero, negative, or undefined.
    *
    * The default is `Duration.Inf` to cache indefinitely.
    */
  def latestCacheExpiry: Duration

  /**
    * Returns a new builder with the specified authentication.
    *
    * The default is [[Auth.None]] for no authentication.
    */
  def withAuth(auth: Auth): SchemaRegistryClientBuilder[F]

  /**
    * Returns a new builder with caching enabled or disabled.
    *
    * Schemas are cached by id and GUID, versions of subjects, and from the
    * results of registering and looking up schemas. Errors are not cached.
    * Latest subject versions are cached based on [[latestCacheExpiry]], by
    * default caching indefinitely, or until a new version of the subject
    * is registered using the client.
    *
    * Caching is enabled by default.
    */
  def withCaching(caching: Boolean): SchemaRegistryClientBuilder[F]

  /**
    * Returns a new builder with the specified latest cache expiry.
    *
    * This only applies when [[caching]] is enabled. Latest versions
    * are cached indefinitely for `Duration.Inf`, and are not cached
    * for durations which are zero, negative, or undefined.
    *
    * The default is `Duration.Inf` to cache indefinitely.
    */
  def withLatestCacheExpiry(latestCacheExpiry: Duration): SchemaRegistryClientBuilder[F]

  /** Creates a [[SchemaRegistryClient]] using the builder settings. */
  def build: F[SchemaRegistryClient[F]]
}

object SchemaRegistryClientBuilder {
  private final case class SchemaRegistryClientBuilderImpl[F[_]](
    override val client: Client[F],
    override val uri: Uri,
    override val auth: Auth,
    override val caching: Boolean,
    override val latestCacheExpiry: Duration
  )(implicit F: Temporal[F])
    extends SchemaRegistryClientBuilder[F] {
    override def withAuth(auth: Auth): SchemaRegistryClientBuilder[F] =
      copy(auth = auth)

    override def withCaching(caching: Boolean): SchemaRegistryClientBuilder[F] =
      copy(caching = caching)

    override def withLatestCacheExpiry(latestCacheExpiry: Duration): SchemaRegistryClientBuilder[F] =
      copy(latestCacheExpiry = latestCacheExpiry)

    override def build: F[SchemaRegistryClient[F]] =
      SchemaRegistryClient.fromBuilder(this)

    override def toString: String =
      s"SchemaRegistryClientBuilder(uri = $uri, auth = $auth, caching = $caching, latestCacheExpiry = $latestCacheExpiry)"
  }

  /**
    * Returns a new [[SchemaRegistryClientBuilder]] using
    * the provided `Client` and base `Uri` for the schema
    * registry to be used.
    */
  def default[F[_]: Temporal](client: Client[F], uri: Uri): SchemaRegistryClientBuilder[F] =
    SchemaRegistryClientBuilderImpl(
      client = client,
      uri = uri,
      auth = Auth.None,
      caching = true,
      latestCacheExpiry = Duration.Inf
    )
}
