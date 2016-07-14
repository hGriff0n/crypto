package crypto.classical;

import crypto.Cipher;
import crypto.utils.{tabula, tabulaSub};

// There's several forms of autokey ciphers
// Basic Autokey from the American Cryptogram Association
    // enc = msg + key
    // map(enc + msg) => ciphertext
class Autokey(key: String) extends Cipher {
    def genKey(msg: String) = 
        (key + msg).substring(0, msg.length)

    def encrypt(msg: String) = {
        val m = msg.replaceAll(" ", "").toUpperCase
        (for (p <- genKey(m).zip(m)) yield tabula(p._1, p._2)).mkString
    }

    def decrypt(msg: String) = {
        var prod = Vector(key)

        // Repeatedly apply a reverse translate on prod.top
        for (group <- msg.grouped(key.length))
            // Decrypt the group using the decrypt key (reverseTabula)
            prod = prod :+ (for (p <- group.zip(prod.last)) yield tabulaSub(p._1, p._2)).mkString

        prod.tail.mkString
    }
}