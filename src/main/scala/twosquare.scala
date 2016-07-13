package crypto.classical;

import crypto.Cipher;
import crypto.utils.{Polybius, mixed};

// Double Playfair cipher, if that's any hint
// Think of having two polybius squares (with different keys) on top of each other
// For each pair of characters in the plaintext, find the location of the first in the upper square and the second in the bottom square
    // AB => U(rowU, colU) + L(rowL, colL)
// These two coordinates define two corners of a rectangle
// The ciphertext is the characters at the other two corners, taking the upper character first
    // U(rowU, colL) + L(rowL, colU) => CD
// There is no special handling if a digraph has the same column
class TwoSquare(uKey: String, lKey: String) extends Cipher {
    protected val upper = new Polybius(5, uKey)
    protected val lower = new Polybius(5, lKey)

    def this() = this(mixed(false), mixed(false))           // mixed isn't being found

    def encrypt(_msg: String) = {
        val msg = (if (_msg.length % 2 == 1) _msg + "X" else _msg).toUpperCase.replace('I', 'J')

        msg.sliding(2, 2).flatMap(pair => {
            val (rowU, colU) = upper.translate(pair(0))
            val (rowL, colL) = lower.translate(pair(1))
            List(upper.translate(rowU, colL), lower.translate(rowL, colU))
        }).mkString
    }

    def decrypt(msg: String) = encrypt(msg)
}

// Four square is the same process as two square, but with four squares <- I'm going to regret this aren't I
class FourSquare(uKey: String, bKey: String, rKey: String, lKey: String) extends TwoSquare(uKey, bKey) {
    private val left = new Polybius(5, mixed(false))
    private val right = new Polybius(5, mixed(false))

    // Upper | Right
    // Left  | Lower

    def this() = this(mixed(false), mixed(false), mixed(false), mixed(false))

    override def encrypt(_msg: String) = {
        val msg = (if (_msg.length % 2 == 1) _msg + "X" else _msg).toUpperCase.replace('I', 'J')

        msg.sliding(2, 2).flatMap(pair => {
            val (rowU, colU) = upper.translate(pair(0))
            val (rowL, colL) = lower.translate(pair(1))
            List(right.translate(rowU, colL), left.translate(rowL, colU))
        }).mkString
    }

    override def decrypt(msg: String) =
        msg.sliding(2, 2).flatMap(pair => {
            val (rowR, colR) = right.translate(pair(0))
            val (rowL, colL) = left.translate(pair(1))
            List(upper.translate(rowR, colL), lower.translate(rowL, colR))
        }).mkString
}