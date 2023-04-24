 scdbpf
========

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

Add the following to your `build.sbt` file:

    libraryDependencies += "io.github.memo33" %% "scdbpf" % "0.2.0"

The latest version is available on [Maven Central](https://mvnrepository.com/artifact/io.github.memo33/scdbpf).


 Documentation
---------------

See here for the current
[documentation](https://memo33.github.io/scdbpf/io/github/memo33/scdbpf/).
It can also be generated via `sbt doc`. The main page contains
some examples to get started. For example, the task of sorting the
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
