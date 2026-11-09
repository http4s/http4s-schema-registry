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
import org.http4s.Status
import scala.util.control.NoStackTrace

/**
  * Error response from a schema registry, including the
  * HTTP status, error code and message of the response.
  */
final class SchemaRegistryError private (
  val status: Status,
  val errorCode: Option[SchemaRegistryErrorCode],
  val message: String
) extends RuntimeException
  with NoStackTrace {
  override def getMessage: String =
    errorCode match {
      case Some(errorCode) =>
        s"Schema registry responded with status ${status.code} and error code ${errorCode.value}: $message"
      case None =>
        s"Schema registry responded with status ${status.code}: $message"
    }

  override def equals(that: Any): Boolean =
    that match {
      case that: SchemaRegistryError =>
        status == that.status && errorCode == that.errorCode && message == that.message
      case _ =>
        false
    }

  override def hashCode: Int =
    (status, errorCode, message).##

  override def toString: String =
    s"SchemaRegistryError(status = $status, errorCode = $errorCode, message = $message)"
}

object SchemaRegistryError {

  /** Returns a new [[SchemaRegistryError]] with the specified details. */
  def apply(
    status: Status,
    errorCode: Option[SchemaRegistryErrorCode],
    message: String
  ): SchemaRegistryError =
    new SchemaRegistryError(status, errorCode, message)

  implicit val schemaRegistryErrorHash: Hash[SchemaRegistryError] =
    Hash.fromUniversalHashCode

  implicit val schemaRegistryErrorShow: Show[SchemaRegistryError] =
    Show.fromToString
}
