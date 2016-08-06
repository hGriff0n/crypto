package crypto.classical

import crypto.utils.{tabula, tabulaSub};
import crypto.utils.CipherString;

// These ciphers scramble the shift based on location
class Trithemius extends crypto.Cipher {
    override def encrypt(msg: String) =
        (for ((c, i) <- msg.ciphertext.zipWithIndex) yield (tabula(c, i % 26), i)).map(_._1).mkString

    override def decrypt(msg: String) = 
        (for ((c, i) <- msg.zipWithIndex) yield (tabulaSub(c, i % 26), i)).map(_._1).mkString.plaintext
}

class Vigenere(key: String) extends crypto.Cipher {
    override def encrypt(msg: String) =
        (for ((c, i) <- msg.ciphertext.zipWithIndex) yield (tabula(c, key(i % key.length)), i)).map(_._1).mkString

    override def decrypt(msg: String) =
        (for ((c, i) <- msg.zipWithIndex) yield (tabulaSub(c, key(i % key.length)), i)).map(_._1).mkString.plaintext
}

class Beaufort(key: String) extends crypto.Cipher {
    override def encrypt(msg: String) = 
        (for ((c, i) <- msg.ciphertext.zipWithIndex) yield (tabulaSub(key(i % key.length), c), i)).map(_._1).mkString
}