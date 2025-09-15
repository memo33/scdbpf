// For publishing to Maven Central, remove `-SNAPSHOT` from version, then
//
//   GPG_TTY=$(tty) sbt +publishSigned
//
// to create staging bundle at target/sonatype-staging/(version). Then
//
//   sbt sonatypeBundleRelease
//
// to upload bundle to Sonatype.
//
// See https://github.com/xerial/sbt-sonatype for details.

publishMavenStyle := true

import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("memo33", "scdbpf", "memo33@users.noreply.github.com"))
