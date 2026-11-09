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
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json

/**
  * Result of checking whether a schema is compatible with a subject version.
  *
  * Compatibility is checked according to the compatibility level for the subject.
  */
sealed abstract class Compatibility {

  /** Whether the schema is compatible with the subject version. */
  def isCompatible: Boolean

  /** For incompatible schemas, optional details of the incompatibilities. */
  def errors: List[String]
}

object Compatibility {
  private case object CompatibleImpl extends Compatibility {
    override val isCompatible: Boolean =
      true

    override val errors: List[String] =
      Nil

    override def toString: String =
      "Compatible"
  }

  private final case class IncompatibleImpl(override val errors: List[String]) extends Compatibility {
    override val isCompatible: Boolean =
      false

    override def toString: String =
      s"Incompatible($errors)"
  }

  /** The schema is compatible with the subject version. */
  val Compatible: Compatibility =
    CompatibleImpl

  /** The schema is incompatible with the subject version. */
  def Incompatible(errors: List[String]): Compatibility =
    IncompatibleImpl(errors)

  implicit val compatibilityDecoder: Decoder[Compatibility] =
    Decoder.instance { c =>
      c.get[Boolean]("is_compatible").flatMap { isCompatible =>
        if (isCompatible) Right(Compatible)
        else
          c.getOrElse[List[Json]]("messages")(Nil).map { messages =>
            Incompatible(messages.map(message => message.asString.getOrElse(message.noSpaces)))
          }
      }
    }

  implicit val compatibilityEncoder: Encoder.AsObject[Compatibility] =
    Encoder.forProduct2("is_compatible", "messages") { (compatibility: Compatibility) =>
      (compatibility.isCompatible, compatibility.errors)
    }

  implicit val compatibilityHash: Hash[Compatibility] =
    Hash.fromUniversalHashCode

  implicit val compatibilityShow: Show[Compatibility] =
    Show.fromToString
}
