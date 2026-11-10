# http4s-schema-registry <img src="docs/img/logo.svg" align="right" height="130" alt="" />

Client for schema registries implementing the [Confluent Schema Registry REST API](https://docs.confluent.io/platform/current/schema-registry/develop/api.html), like [Confluent Schema Registry](https://github.com/confluentinc/schema-registry) and [Karapace](https://github.com/Aiven-Open/karapace).

## Getting Started

Add the following line to your `build.sbt` file, replacing `version` with a [release version](https://github.com/http4s/http4s-schema-registry/releases).

```scala
libraryDependencies += "org.http4s" %% "http4s-schema-registry" % version
```

Published for Scala 2.13 and 3.3, [Scala.js](https://www.scala-js.org) 1.22, and [Scala Native](https://scala-native.org) 0.5.<br>
If you are using Scala.js or Scala Native, then replace `%%` with `%%%`.

## Usage

Create a `SchemaRegistryClient` with `SchemaRegistryClientBuilder`, using an http4s `Client`.

```scala
import cats.effect.IO
import cats.effect.IOApp
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.schemaregistry.Auth
import org.http4s.schemaregistry.Schema
import org.http4s.schemaregistry.SchemaRegistryClientBuilder
import org.http4s.schemaregistry.SubjectName
import org.http4s.syntax.literals.*

object Main extends IOApp.Simple {
  override def run: IO[Unit] =
    EmberClientBuilder.default[IO].build.use { client =>
      for {
        schemaRegistryClient <- SchemaRegistryClientBuilder
          .default(client, uri"http://localhost:8081")
          .withAuth(Auth.Basic("username", "password"))
          .build
        subject = SubjectName("topic-value")
        id <- schemaRegistryClient.registerSchema(subject, Schema("\"string\""))
        schema <- schemaRegistryClient.getSchema(id)
        _ <- IO.println(schema)
      } yield ()
    }
}
```

### Caching

By default, the client caches schemas indefinitely where possible. Use `withCaching(false)` to disable caching entirely.

The cache for the latest version of subjects is invalidated when registering a new schema for the subject. The cache expiry for latest versions can also be changed using `withLatestCacheExpiry`.

### Testing

`TestSchemaRegistry` provides an in-memory registry for tests. Use `client` to get a builder for a client of the registry, and `registered` to get the registered schemas.

```scala
import cats.effect.IO
import org.http4s.schemaregistry.TestSchemaRegistry

for {
  registry <- TestSchemaRegistry[IO]
  client <- registry.client.build
} yield client
```

All schemas are compatible by default, but you can use `TestSchemaRegistry.withCompatibility` to decide which schemas are compatible.

## License

Licensed under the [Apache License, Version 2.0](license.txt).
