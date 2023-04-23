package io.github.memo33.scdbpf;

// copied from jdbpfx.util.DBPFPackager
class QfsDecompression {

    private static void arrayCopy2(byte[] src, int srcPos, byte[] dest, int destPos, int length) {
        for (int i = 0; i < length; i++) {
            dest[destPos + i] = src[srcPos + i];
        }
    }

    private static void offsetCopy(byte[] array, int srcPos, int destPos, int length) {
        srcPos = destPos - srcPos;
        for (int i = 0; i < length; i++) {
            array[destPos + i] = array[srcPos + i];
        }
    }

    public static void decompress(byte[] cData, byte[] dData) {
        assert dData.length <= 0xFFFFFF;
        int dpos = 0;
        // COMPRESSED DATA
        int pos = 9;
        int control1 = 0;
        while (control1 < 0xFC && pos < cData.length) {
            control1 = cData[pos] & 0xFF;
            pos++;

            if (control1 >= 0 && control1 <= 127) {
                // 0x00 - 0x7F
                int control2 = cData[pos] & 0xFF;
                pos++;
                int numberOfPlainText = (control1 & 0x03);
                arrayCopy2(cData, pos, dData, dpos, numberOfPlainText);
                dpos += numberOfPlainText;
                pos += numberOfPlainText;

                int offset = ((control1 & 0x60) << 3) + (control2) + 1;
                int numberToCopyFromOffset = ((control1 & 0x1C) >> 2) + 3;
                offsetCopy(dData, offset, dpos, numberToCopyFromOffset);
                dpos += numberToCopyFromOffset;

            } else if (control1 >= 128 && control1 <= 191) {
                // 0x80 - 0xBF
                int control2 = cData[pos] & 0xFF;
                pos++;
                int control3 = cData[pos] & 0xFF;
                pos++;

                int numberOfPlainText = (control2 >> 6) & 0x03;
                arrayCopy2(cData, pos, dData, dpos, numberOfPlainText);
                dpos += numberOfPlainText;
                pos += numberOfPlainText;

                int offset = ((control2 & 0x3F) << 8) + (control3) + 1;
                int numberToCopyFromOffset = (control1 & 0x3F) + 4;
                offsetCopy(dData, offset, dpos, numberToCopyFromOffset);
                dpos += numberToCopyFromOffset;
            } else if (control1 >= 192 && control1 <= 223) {
                // 0xC0 - 0xDF
                int numberOfPlainText = (control1 & 0x03);
                int control2 = cData[pos] & 0xFF;
                pos++;
                int control3 = cData[pos] & 0xFF;
                pos++;
                int control4 = cData[pos] & 0xFF;
                pos++;
                arrayCopy2(cData, pos, dData, dpos, numberOfPlainText);
                dpos += numberOfPlainText;
                pos += numberOfPlainText;

                int offset = ((control1 & 0x10) << 12) + (control2 << 8) + (control3) + 1;
                int numberToCopyFromOffset = ((control1 & 0x0C) << 6) + (control4) + 5;
                offsetCopy(dData, offset, dpos, numberToCopyFromOffset);
                dpos += numberToCopyFromOffset;
            } else if (control1 >= 224 && control1 <= 251) {
                // 0xE0 - 0xFB
                int numberOfPlainText = ((control1 & 0x1F) << 2) + 4;
                arrayCopy2(cData, pos, dData, dpos, numberOfPlainText);
                dpos += numberOfPlainText;
                pos += numberOfPlainText;
            } else {
                int numberOfPlainText = (control1 & 0x03);
                arrayCopy2(cData, pos, dData, dpos, numberOfPlainText);
                dpos += numberOfPlainText;
                pos += numberOfPlainText;
            }
        }

        if (dpos != dData.length) {
            throw new IndexOutOfBoundsException();
        }
    }
}
