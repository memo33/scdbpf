
/** Provides methods for accessing and modifying the contents of DBPF formatted
  * files. Currently, only DBPF version 1.0 is supported (used by SimCity 4).
  *
  * DBPF files are accessed via [[DbpfFile DbpfFiles]], a container of
  * [[DbpfEntry DbpfEntries]]. The content of a `DbpfEntry` can be accessed from
  * [[BufferedEntry BufferedEntries]] in decoded form as [[DbpfType]], such as
  * `Exemplar`, `Fsh` or `Sc4Path`.
  *
  * This package object provides additional type aliases for `DbpfExceptions`.
  *
  * == Examples ==
  *
  * To get started, it is easiest to start the REPL via `sbt console`, which
  * loads all the dependencies and useful initial import statements. Reading a
  * DBPF file, sorting its entries by TGI and writing back to the same file
  * could be achieved like this:
  *
  * {{{
  * val dbpf = DbpfFile.read(new File("foobar.dat"))
  * dbpf.write(dbpf.entries.sortBy(_.tgi))
  * }}}
  *
  * This example shifts the GIDs of all LTexts by +3:
  *
  * {{{
  * dbpf.write(dbpf.entries.map { e =>
  *   if (e.tgi matches Tgi.LText)
  *     e.copy(e.tgi.copy(gid = e.tgi.gid + 3))
  *   else
  *     e
  * })
  * }}}
  *
  * Another example: This decodes all the `Sc4Path` entries and rotates
  * them by 90 degree.
  *
  * {{{
  * val writeList = for (e <- dbpf.entries) yield {
  *   if (e.tgi matches Tgi.Sc4Path) {
  *     val be = e.toBufferedEntry.convertContentTo(Sc4Path)
  *     be.copy(content = be.content * RotFlip.R1F0)
  *   } else {
  *     e
  *   }
  * }
  * dbpf.write(writeList)
  * }}}
  *
  * The following example finds the first entry that is an exemplar and
  * contains a property with a specific ID. (Note the `view` to avoid
  * unnecessary decoding if the exemplar is already among the first entries.)
  *
  * {{{
  * val id = UInt(0x12345678)
  * dbpf.entries.view.
  *   filter(_.tgi matches Tgi.Exemplar).
  *   map(_.toBufferedEntry.convertContentTo(Exemplar)).
  *   find(_.content.properties.contains(id))
  * }}}
  */
package object scdbpf {

  import DbpfUtil.Converter
  /** provides a converter for `BufferedEntries`, given a converter of `DbpfTypes` */
  implicit def genericConverter[A <: DbpfType](implicit conv: Converter[DbpfType, A]) = new Converter[BufferedEntry[DbpfType], BufferedEntry[A]] {
    def apply(from: BufferedEntry[DbpfType]): BufferedEntry[A] = {
      from.copy(content = conv(from.content))
    }
  }

  /** resolves generalizing conversions from specialized types to general ones */
  implicit def noopConverter[A <: DbpfType] = new Converter[BufferedEntry[A], BufferedEntry[A]] {
    def apply(from: BufferedEntry[A]): BufferedEntry[A] = from
  }

  private[scdbpf] type JFile = java.io.File
  // the following types exist to ease migrating away from rapture-io 0.9.0
  type ExceptionHandler = scdbpf.compat.ExceptionHandler
  val strategy = scdbpf.compat.strategy

  // aliases for convenience
  type DbpfException = DbpfExceptions.DbpfException
  type DbpfIoException = DbpfExceptions.DbpfIoException
  type DbpfFileFormatException = DbpfExceptions.DbpfFileFormatException
  type DbpfDecodeFailedException = DbpfExceptions.DbpfDecodeFailedException
  type DbpfStreamOutOfDateException = DbpfExceptions.DbpfStreamOutOfDateException
}
