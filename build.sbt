name := "scdbpf"

organization := "com.github.memo33"

version := "0.1.4"

scalaVersion := "2.11.2"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  //"-Yinline-warnings",
  "-optimize",
  "-encoding", "UTF-8",
  "-target:jvm-1.6")

initialCommands in console := """
import rapture.io._, rapture.core._
import strategy.throwExceptions
import passera.unsigned._
import scdbpf._
import DbpfUtil._
import java.io.File
"""

autoAPIMappings := true


libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.6"

libraryDependencies += "com.propensive" %% "rapture-io" % "0.9.1"

libraryDependencies += "com.propensive" %% "rapture-core" % "0.9.0"


resolvers += "stephenjudkins-bintray" at "http://dl.bintray.com/stephenjudkins/maven"

libraryDependencies += "ps.tricerato" %% "pureimage" % "0.1.1"


resolvers += "memo33-gdrive-repo" at "https://googledrive.com/host/0B9r6o2oTyY34ZVc4SFBWMV9yb0E/repo/releases/"

libraryDependencies += "com.github.memo33" %% "scala-unsigned" % "0.1.2"

libraryDependencies += "com.github.memo33" % "jsquish" % "0.1"
