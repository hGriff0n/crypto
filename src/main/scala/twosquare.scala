package crypto.classical;

import crypto.Cipher;
import crypto.utils.Polybius;

// TODO: Think of changing polybius to remove q instead of handling i/j

// Double Playfair cipher, if that's any hint
// Think of having two polybius squares (with different keys) on top of each other
// For each pair of characters in the plaintext, find the location of the first in the upper square and the second in the bottom square
    // AB => U(rowU, colU) + L(rowL, colL)
// These two coordinates define two corners of a rectangle
// The ciphertext is the characters at the other two corners, taking the upper character first
    // U(rowU, colL) + L(rowL, colU) => CD

// TODO: Add keys to upper/lower
// TODO: Handle odd length keys
// TODO: Improve implementation
class TwoSquare extends Cipher {
    protected val upper = new Polybius(5, mixed(false))
    protected val lower = new Polybius(5, mixed(false))

    def encrypt(msg: String) = {
        var res = ""
        for (pair <- msg.toUpperCase.sliding(2, 2)) {
            val (rowU, colU) = upper.translate(pair(0))
            val (rowL, colL) = lower.translate(pair(1))
            res += upper.translate(rowU, colL)
            res += lower.translate(rowL, colU)
        }

        res
    }

    def decrypt(msg: String) = encrypt(msg)
}

// Four square is the same as two square, but with four squares <- I'm going to regret this aren't I
class FourSquare extends TwoSquare {
    private val left = new Polybius(5, mixed(false))
    private val right = new Polybius(5, mixed(false))

    // Upper | Right
    // Left  | Lower

    def encrypt(msg: String) = {
        msg
    }

    def decrypt(msg: String) = {
        msg
    }
}