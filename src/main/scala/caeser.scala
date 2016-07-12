package crypto.classical

import crypto.Cipher;
import crypto.utils.tabula;

// If i can define a "CipherApply" trait/ADT, I can move encode to Cipher and
    // implement encrypt and decrypt in the Cipher trait (encode would take a CipherApply)
    // should wait until i get to more complex encryption schemes til I decide
class Caeser(val shft: Int) extends Cipher {
    def encrypt(msg: String): String =
        for (c <- msg.toUpperCase) yield tabula(c, shft)

    def decrypt(msg: String): String =
        for (c <- msg) yield tabula(c, -shft)
}

class Rot13 extends Caeser(13)