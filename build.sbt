name := "scdbpf"

organization := "io.github.memo33"

version := "0.2.2"

ThisBuild / versionScheme := Some("early-semver")

description := "A Scala library for the DBPF file format"

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

crossScalaVersions := List("2.11.12", "2.12.18", "2.13.12", "3.7.3")  // use `sbt +publishLocal` to publish all versions

scalaVersion := crossScalaVersions.value.last

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-encoding", "UTF-8")

scalacOptions ++= CrossVersion.partialVersion(scalaVersion.value).toSeq.flatMap {
  case ((2, v)) if v <= 11 =>
    Seq(
      //"-Yinline-warnings",
      "-optimize",
      "-target:jvm-1.8")
  case ((2, v)) if v >= 12 =>
    Seq(
      // "-opt-warnings:at-inline-failed-summary",
      // "-opt:l:inline", "-opt-inline-from:<sources>",
      "-release:8")
  case ((3, _)) =>
    Seq(
      // "-opt-warnings:at-inline-failed-summary",
      // "-opt:l:inline", "-opt-inline-from:<sources>",
      // "-Wvalue-discard",
      // "-source:future",
      // TODO re-enable silenced deprecation warnings after dropping scala 2
      "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s",
      "-Wconf:msg=it should not be used as infix operator:s",
      "-Wconf:msg=as a type operator has been deprecated; use:s",
      "-Wconf:msg=Ignoring \\[this\\] qualifier:s",
      "-Wconf:msg=you can simply leave out the trailing ` _`:s",
      "-Wconf:msg=is no longer supported for vararg splices:s",
      "-Wconf:msg=`_` is deprecated for wildcard arguments of types:s",
      "-Wconf:msg=method stringPrefix of type => String should be removed or renamed:s",
      "-release:8")
}

javacOptions ++= Seq("--release", "8")

console / initialCommands := """
import io.github.memo33.passera.unsigned._
import io.github.memo33.scdbpf._, strategy.throwExceptions, DbpfUtil._
import java.io.File
"""

Compile / doc / scalacOptions ++= { ((baseDirectory).map { bd =>
  Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/memo33/scdbpf/tree/master€{FILE_PATH}.scala")
}).value }

autoAPIMappings := true


publishTo := sonatypePublishToBundle.value

ThisBuild / sonatypeCredentialHost := xerial.sbt.Sonatype.sonatypeCentralHost  // "central.sonatype.com", previously "s01.oss.sonatype.org"

useGpgPinentry := true  // see https://github.com/sbt/sbt-pgp/issues/178 or https://github.com/microsoft/WSL/issues/4029#issuecomment-491547314

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.3.1" cross CrossVersion.for3Use2_13

libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.9.0"


libraryDependencies += "io.github.memo33" %% "scala-unsigned" % "0.2.0" cross CrossVersion.for3Use2_13

libraryDependencies += "io.github.memo33" % "jsquish" % "2.1.0"
