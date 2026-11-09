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

import org.http4s.AuthScheme
import org.http4s.BasicCredentials
import org.http4s.Credentials
import org.http4s.headers.Authorization

/** Authentication for requests to a schema registry. */
sealed abstract class Auth {
  private[schemaregistry] def authorization: Option[Authorization]
}

object Auth {
  private final case class BasicAuth(username: String, password: String) extends Auth {
    override private[schemaregistry] def authorization: Option[Authorization] =
      Some(Authorization(BasicCredentials(username, password)))

    override def toString: String =
      s"Basic($username, ***)"
  }

  private final case class BearerAuth(token: String) extends Auth {
    override private[schemaregistry] def authorization: Option[Authorization] =
      Some(Authorization(Credentials.Token(AuthScheme.Bearer, token)))

    override def toString: String =
      "Bearer(***)"
  }

  private case object NoAuth extends Auth {
    override private[schemaregistry] def authorization: Option[Authorization] =
      scala.None

    override def toString: String =
      "None"
  }

  /** Authenticates with the specified username and password. */
  def Basic(username: String, password: String): Auth =
    BasicAuth(username, password)

  /** Authenticates with the specified bearer token. */
  def Bearer(token: String): Auth =
    BearerAuth(token)

  /** Does not authenticate. */
  val None: Auth =
    NoAuth
}
