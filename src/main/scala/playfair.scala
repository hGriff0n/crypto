package crypto.classical;

import crypto.Cipher;
import crypto.utils.Polybius;

class Playfair(key: String) extends Cipher {
    val sq = new Polybius(5, key)

    private def encode(pair: String, off: Int): String = {
        val (row0, col0) = sq.translate(pair(0))
        val (row1, col1) = sq.translate(if (pair(0) == pair(1)) 'X' else pair(1))

        if (row0 == row1) {
            "" + sq.translate(row0, (col0 + off) % 5) + sq.translate(row1, (col1 + off) % 5)
        } else if (col0 == col1) {
            "" + sq.translate((row0 + off) % 5, col0) + sq.translate((row1 + off) % 5, col1)
        } else {
            "" + sq.translate(row0, col1) + sq.translate(row1, col0)
        }
    }

    def encrypt(msg: String) = 
        (for (pair <- (if (msg.length % 2 == 1) msg + "X" else msg).toUpperCase.replace('I', 'J').sliding(2, 2))
            yield encode(pair, 1)).mkString

    // Note: This doesn't handle replacing added X's as there's no way to determine
        // which ones where added programatically (in the current framework at least) 
    def decrypt(msg: String) =
        (for (pair <- msg.sliding(2, 2))
            yield encode(pair, 4)).mkString
}