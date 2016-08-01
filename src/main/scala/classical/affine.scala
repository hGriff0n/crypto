package crypto.classical;

import crypto.Cipher;
import crypto.utils.modInv;

// TODO: Is there any way I can improve the error reporting ???
class Affine(a: Int, b: Int) extends Cipher {
    private val inv: Int = modInv(a, 26) match {
        case Some(num) => num
        case None => throw new Exception(s"$a and 26 are not coprime")
    }

    private def apply(to: Int, a: Int, b: Int) = (to * a + b + 26) % 26
    private def encode(c: Char, a: Int, b: Int) = c match {
        case c if lc.contains(c) =>
            (apply(c.toInt - 97, a, b) + 97).toChar
        case c if uc.contains(c) =>
            (apply(c.toInt - 65, a, b) + 65).toChar
        case c => c
    }

    override def encrypt(msg: String) = for (c <- msg) yield encode(c, a, b)
    override def decrypt(msg: String) = for (c <- msg) yield encode(c, inv, inv * -b % 26)
}

class Atbash extends Affine(25, 25)