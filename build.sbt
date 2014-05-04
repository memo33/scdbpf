name := "scdbpf"

version := "0.1.1"

scalaVersion := "2.11.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

packSettings

initialCommands in console := """
import rapture.io._, rapture.core._
import strategy.throwExceptions
import passera.unsigned._
import scdbpf._
import DbpfUtil._
import java.io.File
"""

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.6"

libraryDependencies += "jsquish" % "jsquish" % "0.1" from "https://dl.dropboxusercontent.com/s/7ijtzyjb353fyas/jsquish.jar"

libraryDependencies += "com.propensive" %% "rapture-io" % "0.9.1"

libraryDependencies += "com.propensive" %% "rapture-core" % "0.9.0"

libraryDependencies += "passera.unsigned" %% "scala-unsigned" % "0.1.1" from "https://dl.dropboxusercontent.com/s/yojvk2bb7o1c627/scala-unsigned_2.11-0.1.1.jar"

resolvers += "stephenjudkins-bintray" at "http://dl.bintray.com/stephenjudkins/maven"

libraryDependencies += "ps.tricerato" %% "pureimage" % "0.1.1"

autoAPIMappings := true
