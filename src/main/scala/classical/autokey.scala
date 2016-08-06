package crypto.classical;

import crypto.utils.{tabula, tabulaSub};

// There's several forms of autokey ciphers
// Basic Autokey from the American Cryptogram Association
    // enc = msg + key
    // map(enc + msg) => ciphertext
class Autokey(key: String) extends crypto.Cipher {
    def genKey(msg: String) = 
        (key + msg).substring(0, msg.length)

    override def encrypt(msg: String) = {
        val m = msg.replaceAll(" ", "").toUpperCase
        (for (p <- genKey(m).zip(m)) yield tabula(p._1, p._2)).mkString
    }

    override def decrypt(msg: String) = {
        var prod = Vector(key)

        // To decrypt, you need to incrementally figure out the encryption key
            // The first part of the encryption key is the cipher key
            // Unapply that key to the first part of the message to get the next part
            // Combine the keys created from deciphering the message to get the plaintext
        for (group <- msg.grouped(key.length))
            prod = prod :+ (for (p <- group.zip(prod.last)) yield tabulaSub(p._1, p._2)).mkString

        prod.tail.mkString
    }
}