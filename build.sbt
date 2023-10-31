/*
 * Copyright (c) 2020 Jobial OÜ. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"). You may not use this file except in compliance with
 * the License. A copy of the License is located at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * or in the "license" file accompanying this file. This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
name := "scala-cdktf"

ThisBuild / organization := "io.jobial"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.15", "2.13.8")
ThisBuild / version := "0.8.5"
ThisBuild / scalacOptions += "-target:jvm-1.8"
ThisBuild / javacOptions ++= Seq("-source", "11", "-target", "11")
ThisBuild / Test / packageBin / publishArtifact := true
ThisBuild / Test / packageSrc / publishArtifact := true
ThisBuild / Test / packageDoc / publishArtifact := true
ThisBuild / resolvers += "Mulesoft" at "https://repository.mulesoft.org/nexus/content/repositories/public/"

import sbt.Keys.{description, libraryDependencies, publishConfiguration}
import sbt.addCompilerPlugin
import xerial.sbt.Sonatype._

lazy val commonSettings = Seq(
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  publishM2Configuration := publishM2Configuration.value.withOverwrite(true),
  publishTo := publishTo.value.orElse(sonatypePublishToBundle.value),
  sonatypeProjectHosting := Some(GitHubHosting("jobial-io", "scala-cdktf", "orbang@jobial.io")),
  organizationName := "Jobial OÜ",
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  description := "Write Terraform CDK (cdktf) in functional Scala",
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  scalacOptions ++= (if (scalaBinaryVersion.value != "2.13") Seq("-Ypartial-unification") else Seq())
)

lazy val CatsVersion = "2.0.0"
lazy val CatsTestkitScalatestVersion = "1.0.0-RC1"
lazy val ScalaLoggingVersion = "3.9.2"
lazy val ScalatestVersion = "3.2.3"
lazy val SourcecodeVersion = "0.2.3"
lazy val CdktfProviderAwsVersion = "16.0.5"
lazy val SclapVersion = "1.3.9"
lazy val SprintVersion = "0.0.9"
lazy val CirceVersion = "0.12.0-M3"
lazy val LogbackVersion = "1.2.3"
lazy val ShapelessVersion = "2.3.3"
lazy val ProguardVersion = "7.2.2"
lazy val ScaseVersion = "1.3.14"

lazy val root: Project = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.typelevel" %% "cats-effect" % CatsVersion,
      "org.typelevel" %% "cats-testkit-scalatest" % CatsTestkitScalatestVersion % Test,
      "org.typelevel" %% "kittens" % CatsVersion % Test,
      "com.typesafe.scala-logging" %% "scala-logging" % ScalaLoggingVersion,
      "com.lihaoyi" %% "sourcecode" % SourcecodeVersion,
      "org.scalatest" %% "scalatest" % ScalatestVersion % Test,
      "ch.qos.logback" % "logback-classic" % LogbackVersion % Test,
      "com.chuusai" %% "shapeless" % ShapelessVersion,
      "com.github.sbt" % "junit-interface" % "0.13.2" % Test,
      "com.hashicorp" % "cdktf-provider-aws" % CdktfProviderAwsVersion,
      "io.circe" %% "circe-generic-extras" % CirceVersion,
      "io.jobial" %% "sclap" % SclapVersion,
      "io.jobial" %% "sprint" % SprintVersion,
      "io.jobial" %% "scase-aws" % ScaseVersion
    )
  )

