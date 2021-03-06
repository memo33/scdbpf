 scdbpf
========

[ ![Download](https://api.bintray.com/packages/memo33/maven/scdbpf/images/download.svg) ](https://bintray.com/memo33/maven/scdbpf/_latestVersion)
[![Build Status](https://travis-ci.org/memo33/scdbpf.svg?branch=master)](https://travis-ci.org/memo33/scdbpf)

This is a DBPF library written in Scala, which runs on the JVM. Its most
important features are immutability, which makes the library thread-safe,
as well as a simple API, which also makes it particularly suitable for
scripting.

Currently, only DBPF format version 1.0 is supported, which is used by
SimCity 4. The most common DBPF types are supported, such as Exemplar,
Cohort, SC4Paths, S3D, FSH, LText.

This library has evolved from the
[jDBPFX](https://github.com/memo33/jdbpfx) library; yet, almost everything
has been rewritten entirely.


 Installation
--------------

For automatic dependency management, add the following to your sbt build file:

    resolvers += "memo33-bintray" at "https://dl.bintray.com/memo33/maven"

    libraryDependencies += "com.github.memo33" %% "scdbpf" % "0.1.7"

(Replace version number by latest release tag.)


 Documentation
---------------

The current ScalaDocs can be found
[here](https://memo33.github.io/scdbpf/#scdbpf.package)
or can be generated via `sbt doc`. The main page also contains
some examples to get started. For example, the simple task of sorting the
entries of a DBPF file by TGI would be achieved via `sbt console` like
this:

    val dbpf = DbpfFile.read(new File("foobar.dat"))
    dbpf.write(dbpf.entries.sortBy(_.tgi))


 Contact and Support
---------------------

Support is provided at
[SC4Devotion.com](http://sc4devotion.com/forums/index.php?topic=16491).

The source files can be found at
[GitHub.com](https://github.com/memo33/scdbpf).


 License
---------

This library is released under the MIT license (see included license file).
