
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

For automatic dependency management, add the following to your sbt build file:

    resolvers += "memo33-gdrive-repo" at "https://googledrive.com/host/0B9r6o2oTyY34ZVc4SFBWMV9yb0E/repo/releases/"

    libraryDependencies += "com.github.memo33" %% "scdbpf" % "0.1.4"

(Replace version number by latest release tag.)


 Documentation
---------------

The ScalaDocs can be found
[here](http://memo33.github.io/scdbpfdoc/api/index.html#scdbpf.package),
can be downloaded from the above repository
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
