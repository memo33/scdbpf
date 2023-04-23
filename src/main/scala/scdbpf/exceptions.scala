package io.github.memo33
package scdbpf

import java.io.IOException

object DbpfExceptions {

  sealed trait DbpfException extends Exception

  class DbpfIoException(msg: String = "") extends IOException(msg) with DbpfException

  class DbpfFileFormatException(msg: String = "") extends DbpfIoException(msg)

  class DbpfStreamOutOfDateException(msg: String = "") extends DbpfIoException(msg)

  class DbpfDecodeFailedException(msg: String = "", cause: Throwable = null) extends Exception(msg, cause) with DbpfException

}
