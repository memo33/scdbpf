package scdbpf

import scala.annotation.tailrec

/* This code was loosely adapted from different sources:
 * - http://www.wiki.sc4devotion.com/index.php?title=DBPF_Compression
 * - https://github.com/memo33/jDBPFX/blob/master/src/jdbpfx/util/DBPFPackager.java
 * and compression code written by B. Rudiak-Gould
 * - http://www.moreawesomethanyou.com/smf/index.php/topic,8279.0.html
 * (the GPLv2 probably still applies here)
 */
private object QfsCompression {

  val CompressionHeaderSize = 9
  val LookAhead = 3
  val MinMatch = 3
  val MaxMatch = 1028
  val MaxIter = 0x80

  private class Hash(sizeHint: Int) {

    val WSize = 0x20000 min Integer.highestOneBit(sizeHint)
    val WMask = WSize - 1
    val HashSize = 0x10000 min (0x20 max (Integer.highestOneBit(sizeHint) / 2))
    val HashMask = HashSize - 1
    val HashShift = (Integer.numberOfTrailingZeros(HashSize) + LookAhead - 1) / LookAhead

    private var hash = 0
    private val prev = new Array[Int](WSize)
    private val head = Array.fill[Int](HashSize)(-1)

    @inline def previous(pos: Int) = prev(pos & WMask)
    @inline def update(c: Int): Unit = hash = ((hash << HashShift) ^ c) & HashMask
    @inline def insert(pos: Int): Unit = {
      prev(pos & WMask) = head(hash)
      head(hash) = pos
    }
  }

  private object DestinationIsFull extends scala.util.control.ControlThrowable

  private class SlidingWindow(val arr: Array[Byte]) {
    var pos = 0
    @inline def apply(i: Int) = arr(pos + i)
    @inline def inc() = { pos += 1 }
    @inline def move(i: Int) = { pos += i }
    @inline def put(x: Int) = { arr(pos) = x.toByte; inc() }
    @inline def remaining: Int = arr.length - pos
  }

  def compress(srcArr: Array[Byte]): Array[Byte] = try {
    assert(CompressionHeaderSize < srcArr.length && srcArr.length <= 0xFFFFFF)
    val src = new SlidingWindow(srcArr)
    val destArr = new Array[Byte](srcArr.length + 4) // we don't want to make result larger than src anyway; 4 bytes for maximum code point size
    val dest = new SlidingWindow(destArr)
    dest.move(CompressionHeaderSize)
    val matchWindow = new SlidingWindow(srcArr)

    val hash = new Hash(srcArr.length)
    val MaxDist = hash.WSize

    hash.update(src(0))
    hash.update(src(1))
    hash.update(src(2))
    assert(LookAhead == 3)
    hash.insert(src.pos)

    /* definition of closures */

    // adds next byte to hash
    @inline def hashUpdate() = {
      hash.update(src(LookAhead - 1))
      hash.insert(src.pos)
    }

    @inline def commonPrefix(a: SlidingWindow, b: SlidingWindow): Int = {
      assert(a.pos < b.pos && (a.arr eq b.arr))
      var i = 0
      while(b.pos + i < b.arr.length && a(i) == b(i)) i += 1
      i
    }

    // does not(!) check _all_ the constraints
    @inline def hasCodepoint(offset: Int, len: Int): Boolean = {
      len >= 3 && offset < 1024 ||
      len >= 4 && offset < 0x4000 ||
      len >= 5 && offset < 0x20000
    }

    // returns best length and additionally moves matchWindow to best position
    @inline @tailrec
    def findLength(bestLen: Int, bestPos: Int, iter: Int): Int = {
      if (iter > MaxIter ||
        bestLen == MaxMatch ||
        src.pos + bestLen == src.arr.length || {
          matchWindow.pos = hash.previous(matchWindow.pos) // assignment!
          matchWindow.pos < 0 || src.pos - matchWindow.pos > MaxDist
        }) {
        // then break out
        matchWindow.pos = bestPos
        bestLen
      } else {
        val len = if (
          bestLen < 2 ||
          matchWindow(bestLen)     == src(bestLen) &&
          matchWindow(bestLen - 1) == src(bestLen - 1)
        ) {
          commonPrefix(matchWindow, src)
        } else {
          0
        }
        if (len > bestLen && hasCodepoint(src.pos - matchWindow.pos - 1, len))
          findLength(len min MaxMatch, matchWindow.pos, iter + 1)
        else
          findLength(bestLen, bestPos, iter + 1)
      }
    }

    // copies len bytes from src to dest and moves dest accordingly
    @inline def transfer(len: Int, srcPos: Int): Unit = {
      System.arraycopy(src.arr, srcPos, dest.arr, dest.pos, len)
      dest.move(len)
    }

    // encodes the (plainLength, copyLength, copyOffset) triple into dest
    // and also transfers the plain text bytes;
    // copyOffset is determined from matchWindow position
    @inline @tailrec
    def encode(plainLength: Int, copyLength: Int): Unit = {
      if (plainLength + 4 > dest.remaining) {
        throw DestinationIsFull
      }
      if (plainLength >= 4) {
        if (plainLength > 112) {
          dest put 0xFB  // single byte code point
          transfer(112, src.pos - plainLength)
          encode(plainLength - 112, copyLength)
        } else {
          dest put (plainLength / 4) + 0xDF  // single byte code point
          transfer(plainLength & ~0x03, src.pos - plainLength)
          encode(plainLength & 0x03, copyLength)
        }
      } else if (copyLength == 0) {
        dest put 0xFC + plainLength // termination byte
        if (plainLength > 0) {
          transfer(plainLength, src.pos - plainLength)
        }
      } else {
        // determine code points and transfer plain text
        val offset = src.pos - matchWindow.pos - 1 // offset is always from right to left
        assert(offset >= 0, s"offset $offset copyLength $copyLength plainLength $plainLength")
        if (offset < 1024 && 3 <= copyLength && copyLength <= 10) {
          dest put ((offset >> 3) & 0x60) + ((copyLength - 3) * 4) + plainLength
          dest put offset
        } else if (offset < 0x4000 && 4 <= copyLength && copyLength <= 67) {
          dest put 0x80 + (copyLength - 4)
          dest put plainLength * 0x40 + (offset >> 8)
          dest put offset
        } else if (offset < 0x20000 && 5 <= copyLength && copyLength <= 1028) {
          dest put 0xC0 + ((offset >> 12) & 0x10) + (((copyLength - 5) >> 6) & 0x0C) + plainLength
          dest put offset >> 8
          dest put offset
          dest put copyLength - 5
        } else {
          // findLength should ensure that this does not happen
          throw new AssertionError(s"no suitable code point: offset $offset copyLength $copyLength plainLength $plainLength")
        }
        if (plainLength > 0) {
          transfer(plainLength, src.pos - plainLength)
        }
      }
    }

    assert(LookAhead == 3 && MinMatch == 3)
    // moves the main sliding window forward
    @inline @tailrec
    def scan(plainStart: Int): Unit = {
      if (src.remaining <= 3) {
        src.move(src.remaining)
        encode(src.pos - plainStart, 0)
      } else {
        matchWindow.pos = src.pos // adjust pos for find length's hash.prev iteration
        val copyLength = findLength(0, src.pos, 0)
        if (copyLength < MinMatch) {
          src.inc()
          hashUpdate()
          scan(plainStart)
        } else {
          encode(src.pos - plainStart, copyLength)
          var i = 0
          while (i < copyLength) {
            src.inc()
            if (src.remaining >= LookAhead)
              hashUpdate()
            i += 1
          }
          scan(src.pos)
        }
      }
    }

    /* end of definition of closures */

    scan(src.pos) // <-- main method invocation

    val destBuf = DbpfUtil.wrapLEBB(destArr)
    destBuf.putInt(dest.pos)
    destBuf.putShort(DbpfUtil.MagicNumber.QFS)
    destBuf.put((srcArr.length >> 16).toByte).put((srcArr.length >> 8).toByte).put((srcArr.length).toByte)
    java.util.Arrays.copyOfRange(destArr, 0, dest.pos)
  } catch {
    case DestinationIsFull => srcArr
  }
}
