name := "scdbpf"

organization := "com.github.memo33"

version := "0.1.7"

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

scalaVersion := "2.11.8"

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

scalacOptions in (Compile, doc) <++= (baseDirectory).map { bd =>
  Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/memo33/scdbpf/tree/master€{FILE_PATH}.scala")
}

autoAPIMappings := true


libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.6"

libraryDependencies += "com.propensive" %% "rapture-io" % "0.9.1"

libraryDependencies += "com.propensive" %% "rapture-core" % "0.9.0"


resolvers += "stephenjudkins-bintray" at "https://dl.bintray.com/stephenjudkins/maven"

libraryDependencies += "ps.tricerato" %% "pureimage" % "0.1.1"


resolvers += "memo33-bintray" at "https://dl.bintray.com/memo33/maven"

libraryDependencies += "com.github.memo33" %% "scala-unsigned" % "0.1.3"

libraryDependencies += "com.github.memo33" % "jsquish" % "2.0.1"
