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

/** Selects a version of a subject, either the latest version or an exact version. */
sealed abstract class VersionSelector {

  /** The exact version which is selected, or `None` if the latest version is selected. */
  def exactVersion: Option[SubjectVersion]
}

object VersionSelector {
  private case object LatestSelector extends VersionSelector {
    override def exactVersion: Option[SubjectVersion] =
      None

    override def toString: String =
      "Latest"
  }

  private final case class ExactSelector(version: SubjectVersion) extends VersionSelector {
    override def exactVersion: Option[SubjectVersion] =
      Some(version)

    override def toString: String =
      s"Exact($version)"
  }

  /** Selects the latest version of a subject. */
  val Latest: VersionSelector =
    LatestSelector

  /** Selects the specified version of a subject. */
  def Exact(version: SubjectVersion): VersionSelector =
    ExactSelector(version)

  implicit val versionSelectorHash: Hash[VersionSelector] =
    Hash.fromUniversalHashCode

  implicit val versionSelectorShow: Show[VersionSelector] =
    Show.fromToString
}
