package crypto.classical

import crypto.Cipher;
import crypto.utils.{tabula, tabulaSub};

// If i can define a "CipherApply" trait/ADT, I can move encode to Cipher and
    // implement encrypt and decrypt in the Cipher trait (encode would take a CipherApply)
    // should wait until i get to more complex encryption schemes til I decide
class Caeser(shft: Int) extends Cipher {
    override def encrypt(msg: String) =
        for (c <- msg.toUpperCase) yield tabula(c, shft)

    override def decrypt(msg: String) =
        for (c <- msg) yield tabulaSub(c, shft)
}

class Rot13 extends Caeser(13)

class Trithemius extends Cipher {
    override def encrypt(msg: String) =
        (for ((c, i) <- msg.toUpperCase.zipWithIndex) yield (tabula(c, i % 26), i)).map(_._1).mkString

    override def decrypt(msg: String) = 
        (for ((c, i) <- msg.zipWithIndex) yield (tabulaSub(c, i % 26), i)).map(_._1).mkString
}

class Vigenere(key: String) extends Cipher {
    override def encrypt(msg: String) =
        (for ((c, i) <- msg.toUpperCase.replaceAll(" ", "").zipWithIndex) yield (tabula(c, key(i % key.length)), i)).map(_._1).mkString

    override def decrypt(msg: String) =
        (for ((c, i) <- msg.zipWithIndex) yield (tabulaSub(c, key(i % key.length)), i)).map(_._1).mkString
}

class Beaufort(key: String) extends Cipher {
    override def encrypt(msg: String) = 
        (for ((c, i) <- msg.toUpperCase.replaceAll(" ", "").zipWithIndex) yield (tabulaSub(key(i % key.length), c), i)).map(_._1).mkString
}