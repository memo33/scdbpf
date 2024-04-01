name := "scdbpf"

organization := "io.github.memo33"

version := "0.2.1"

ThisBuild / versionScheme := Some("early-semver")

description := "A Scala library for the DBPF file format"

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

crossScalaVersions := List("2.11.12", "2.12.18", "2.13.12")  // use `sbt +publishLocal` to publish all versions

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

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "com.michaelpollmeier" %% "scala-arm" % "2.1"  // forked from com.jsuereth to support scala-2.13

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.9.0"


libraryDependencies += "io.github.memo33" %% "scala-unsigned" % "0.2.0"

libraryDependencies += "io.github.memo33" % "jsquish" % "2.1.0"
