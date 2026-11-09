val caseInsensitiveVersion = "1.5.0"
val catsEffectVersion = "3.7.1"
val catsVersion = "2.13.0"
val circeVersion = "0.14.17"
val fs2Version = "3.14.0"
val http4sVersion = "0.23.38"
val kafkaImage = "apache/kafka:4.4.0"
val karapaceImage = "ghcr.io/aiven-open/karapace:6.2.4"
val scala213Version = "2.13.18"
val scala3Version = "3.3.9"
val schemaRegistryImage = "confluentinc/cp-schema-registry:8.4.0"
val testcontainersVersion = "2.0.5"
val vaultVersion = "3.7.0"
val weaverVersion = "0.13.0"

inThisBuild(
  Seq(
    crossScalaVersions := Seq(scala213Version, scala3Version),
    developers := List(tlGitHubDev("vlovgr", "Viktor Rudebeck")),
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
    licenses := Seq(License.Apache2),
    scalaVersion := scala3Version,
    startYear := Some(2026),
    tlBaseVersion := "1.0"
  )
)

lazy val root = tlCrossRootProject
  .aggregate(core, tests)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .settings(
    name := "http4s-schema-registry",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % fs2Version,
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-jawn" % circeVersion,
      "org.http4s" %%% "http4s-client" % http4sVersion,
      "org.http4s" %%% "http4s-core" % http4sVersion,
      "org.typelevel" %%% "case-insensitive" % caseInsensitiveVersion,
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-effect-kernel" % catsEffectVersion,
      "org.typelevel" %%% "cats-effect-std" % catsEffectVersion,
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
      "org.typelevel" %%% "cats-kernel" % catsVersion,
      "org.typelevel" %%% "vault" % vaultVersion
    )
  )

lazy val tests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core)
  .settings(
    name := "http4s-schema-registry-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect-testkit" % catsEffectVersion % Test,
      "org.typelevel" %%% "weaver-cats" % weaverVersion % Test,
      "org.typelevel" %%% "weaver-scalacheck" % weaverVersion % Test
    )
  )
  .jvmSettings(
    Test / fork := true,
    Test / javaOptions ++= Seq(
      s"-Dhttp4s.schemaregistry.test.kafka.image=$kafkaImage",
      s"-Dhttp4s.schemaregistry.test.karapace.image=$karapaceImage",
      s"-Dhttp4s.schemaregistry.test.schemaregistry.image=$schemaRegistryImage"
    ),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-client" % http4sVersion % Test,
      "org.testcontainers" % "testcontainers-kafka" % testcontainersVersion % Test
    )
  )
