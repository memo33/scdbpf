name := "scdbpf"

organization := "com.github.memo33"

version := "0.1.8"

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
import rapture.io._, rapture.core._
import strategy.throwExceptions
import passera.unsigned._
import scdbpf._
import DbpfUtil._
import java.io.File
"""

Compile / doc / scalacOptions ++= { ((baseDirectory).map { bd =>
  Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/memo33/scdbpf/tree/master€{FILE_PATH}.scala")
}).value }

autoAPIMappings := true


libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.6"

libraryDependencies += "com.propensive" %% "rapture-io" % "0.9.1"

libraryDependencies += "com.propensive" %% "rapture-core" % "0.9.0"


libraryDependencies += "ps.tricerato" %% "pureimage" % "0.1.1" from "https://github.com/memo33/scdbpf/releases/download/v0.1.7/pureimage_2.11-0.1.1.jar"

libraryDependencies += "com.github.memo33" %% "scala-unsigned" % "0.1.3" from "https://github.com/memo33/scala-unsigned/releases/download/v0.1.3/scala-unsigned_2.11-0.1.3.jar"

libraryDependencies += "com.github.memo33" % "jsquish" % "2.0.1" from "https://github.com/memo33/jsquish/releases/download/v2.0.1/jsquish-2.0.1.jar"
