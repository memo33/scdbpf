name := "scdbpf"

organization := "com.github.memo33"

version := "0.2.0-SNAPSHOT"

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

scalaVersion := "2.11.12"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  //"-Yinline-warnings",
  "-optimize",
  "-encoding", "UTF-8",
  "-target:jvm-1.7")

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

console / initialCommands := """
import io.github.memo33.passera.unsigned._
import scdbpf._, strategy.throwExceptions, DbpfUtil._
import java.io.File
"""

Compile / doc / scalacOptions ++= { ((baseDirectory).map { bd =>
  Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/memo33/scdbpf/tree/master€{FILE_PATH}.scala")
}).value }

autoAPIMappings := true


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "com.michaelpollmeier" %% "scala-arm" % "2.1"  // forked from com.jsuereth to support scala-2.13

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.3.1"


libraryDependencies += "io.github.memo33" %% "scala-unsigned" % "0.2.0"

libraryDependencies += "com.github.memo33" % "jsquish" % "2.0.1" from "https://github.com/memo33/jsquish/releases/download/v2.0.1/jsquish-2.0.1.jar"
