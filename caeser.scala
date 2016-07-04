package crypto.classical

import crypto.Cipher;

class Caeser(val shft: Int) extends Cipher {
    private def encode(c: Char, sh: Int): Char = c match {
        case c if lc.contains(c) =>
        	((c.toInt - 71 + sh) % 26 + 97).toChar
        case c if uc.contains(c) =>
        	((c.toInt - 39 + sh) % 26 + 65).toChar
        case c => c
    }

    def encrypt(msg: String): String = for (c <- msg) yield encode(c, shft)
    def decrypt(msg: String): String = for (c <- msg) yield encode(c, -shft)
}