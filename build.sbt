name := "scdbpf"

version := "0.1"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

packSettings

initialCommands in console := """
import rapture.io._
import strategy.throwExceptions
import passera.unsigned._
import scdbpf._
import DbpfUtil._
"""

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.2" % "test"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.6"

libraryDependencies += "jsquish" % "jsquish" % "0.1" from "https://dl.dropboxusercontent.com/s/7ijtzyjb353fyas/jsquish.jar"

libraryDependencies += "rapture.io" %% "io" % "0.8.1" from "https://dl.dropboxusercontent.com/s/tcug50wqpst7p5g/io-0.8.1.jar"

libraryDependencies += "passera.unsigned" %% "scala-unsigned" % "0.1" from "https://dl.dropboxusercontent.com/s/zx0oc3lcyc4zd6o/scala-unsigned_2.10-0.1.jar"

resolvers += "stephenjudkins-bintray" at "http://dl.bintray.com/stephenjudkins/maven"

libraryDependencies += "ps.tricerato" %% "pureimage" % "0.1.0"

autoAPIMappings := true
