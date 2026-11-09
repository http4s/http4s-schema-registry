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
import cats.effect.Resource
import com.github.dockerjava.api.command.CreateContainerCmd
import java.time.Duration
import org.http4s.Uri
import org.testcontainers.containers.GenericContainer
import org.testcontainers.containers.Network
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.kafka.KafkaContainer
import org.testcontainers.utility.DockerImageName

object Containers {

  /** Confluent Schema Registry, returning its base URI. */
  def confluent: Resource[IO, Uri] =
    registry(Wait.forHttp("/subjects").forStatusCode(200)) { network =>
      new RegistryContainer(image("schemaregistry"))
        .withNetwork(network)
        .withEnv("SCHEMA_REGISTRY_HOST_NAME", "schema-registry")
        .withEnv("SCHEMA_REGISTRY_LISTENERS", s"http://0.0.0.0:$RegistryPort")
        .withEnv("SCHEMA_REGISTRY_KAFKASTORE_BOOTSTRAP_SERVERS", s"PLAINTEXT://$KafkaListener")
    }

  /**
    * Karapace, returning its base URI.
    *
    * Karapace serves requests before it has been elected primary,
    * and then waits a while before registering schemas, so it is
    * ready when its health status says that it is the primary.
    */
  def karapace: Resource[IO, Uri] =
    registry(
      Wait
        .forHttp("/_health")
        .forStatusCode(200)
        .forResponsePredicate(_.matches("(?s).*\"schema_registry_is_primary\"\\s*:\\s*true.*"))
    ) { network =>
      new RegistryContainer(image("karapace"))
        .withNetwork(network)
        .withCreateContainerCmdModifier { (cmd: CreateContainerCmd) =>
          val _ = cmd.withEntrypoint("python3", "-m", "karapace")
        }
        .withEnv("KARAPACE_KARAPACE_REGISTRY", "true")
        .withEnv("KARAPACE_BOOTSTRAP_URI", KafkaListener)
        .withEnv("KARAPACE_HOST", "0.0.0.0")
        .withEnv("KARAPACE_PORT", RegistryPort.toString)
        .withEnv("KARAPACE_ADVERTISED_HOSTNAME", "karapace")
        .withEnv("KARAPACE_TOPIC_NAME", "_schemas")
        .withEnv("KARAPACE_GROUP_ID", "karapace")
        .withEnv("KARAPACE_MASTER_ELIGIBILITY", "true")
        .withEnv("KARAPACE_COMPATIBILITY", "BACKWARD")
    }

  final class RegistryContainer(image: DockerImageName) extends GenericContainer[RegistryContainer](image)

  private val KafkaListener: String =
    "kafka:19092"

  private val RegistryPort: Int =
    8081

  /** Starts the registry, along with Kafka, where the registry is started when `ready` is satisfied. */
  private def registry(ready: HttpWaitStrategy)(container: Network => RegistryContainer): Resource[IO, Uri] =
    for {
      network <- Resource.fromAutoCloseable(IO(Network.newNetwork()))
      _ <- start {
        new KafkaContainer(image("kafka"))
          .withNetwork(network)
          .withNetworkAliases("kafka")
          .withListener(KafkaListener)
      }
      registry <- start {
        container(network)
          .withExposedPorts(RegistryPort)
          .waitingFor(ready.withStartupTimeout(Duration.ofMinutes(3)))
      }
      uri <- Resource.eval {
        IO.fromEither(Uri.fromString(s"http://${registry.getHost}:${registry.getMappedPort(RegistryPort)}"))
      }
    } yield uri

  private def start[C <: GenericContainer[?]](container: C): Resource[IO, C] =
    Resource.make(IO.blocking { container.start(); container })(container => IO.blocking(container.stop()))

  private def image(name: String): DockerImageName = {
    val property = s"http4s.schemaregistry.test.$name.image"
    DockerImageName.parse(
      sys.props.getOrElse(property, sys.error(s"$property is not set, run the tests via sbt"))
    )
  }
}
