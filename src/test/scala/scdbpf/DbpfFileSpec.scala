package io.github.memo33
package scdbpf

import org.scalatest.CancelAfterFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import strategy.throwExceptions

class DbpfFileSpec extends AnyWordSpec with Matchers with CancelAfterFailure {

  "DbpfFile" should {
    val tmpFile = java.io.File.createTempFile("dbpffile-", ".dat", new java.io.File("target"))
    tmpFile.deleteOnExit()
    val ltext = LText("Hello, world.")
    val tgi = Tgi(0, 0, 0x42).copy(Tgi.LText)

    "support write operation" in {
      val entries = List(BufferedEntry(tgi, ltext, compressed = true))
      DbpfFile.write(entries, tmpFile)
    }

    "support read operation" in {
      val d = DbpfFile.read(tmpFile)
      d.entries.length should be (1)
      d.entries(0).tgi should be (tgi)
      d.entries(0).toBufferedEntry.convertContentTo(LText).content.text should be (ltext.text)
    }

    "support update operation" in {
      val d = DbpfFile.read(tmpFile)
      val e = BufferedEntry(tgi.copy(iid = 0x43), ltext, compressed = true)
      d.write(entries = d.entries ++ Seq(e))
      val d2 = DbpfFile.read(tmpFile)
      d2.entries.length should be (d.entries.length + 1)
    }
  }
}
