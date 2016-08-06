package crypto.classical.mapped

import crypto.utils.{shuffled, columnTranspose}
import crypto.utils.CipherString

// A cubic version of a polybius square is created from a mixed alphabet (3x3x3)
    // Really just three tables with 9 letters in each
    // Commonly a map of character => trifid coord is made
// The coordinates for each character are written out in columns and then read in rows
// Before being divided into triplets and translated back through the table

// Trifid is a slightly different version of a mapped cipher, the mapping doesn't directly produce the ciphering (there's some dispersion)
    // However, the mapping is very important to the operation of the cipher itself
class Trifid extends Cipher[String] {
    encMap = shuffled("ABCDEFGHIJKLMNOPQRSTUVWXYZ.").zipWithIndex.map(p => p._1 -> "%d%d%d".format(p._2 / 9, (p._2 % 9) / 3, p._2 % 9 % 3)).toMap
    decMap = encMap.map(_.swap)

    private def translate(msg: String, group: Int) =
        columnTranspose(msg.ciphertext.map(encMap(_)).toList.flatten, group)
            .map(t => for (trip <- t.sliding(3, 3)) yield decMap(trip.mkString))
            .map(_.mkString).mkString
        
    // mapdec can't be implemented for Trifid due to dispersion effects of the cipher
    override def mapdec(msg: List[String]) = null

    override def encrypt(msg: String) = translate(msg, 3)
    override def decrypt(msg: String) = translate(msg, msg.length).plaintext
}