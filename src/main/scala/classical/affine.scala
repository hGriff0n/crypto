package crypto.classical;

import crypto.utils.modInv;
import crypto.utils.CipherString;

// TODO: Is there any way I can improve the error reporting ???
class Affine(a: Int, b: Int) extends Cipher {
    private val inv: Int = modInv(a, 26) match {
        case Some(num) => num
        case None => throw new Exception(s"$a and 26 are not coprime")
    }

    encMap = uc.map(c => c -> encode(c, a, b).toChar).toMap
    decMap = uc.map(c => c -> encode(c, inv, inv * -b % 26).toChar).toMap

    private def encode(c: Char, a: Int, b: Int) = (((c.toInt - 65) * a + b + 26) % 26) + 'A'
}

class Atbash extends Affine(25, 25)

// mapping = (Map[Char, String], Map[Char, String]
// mapenc = List[(Char, Char)]
// mapdec = List[(Char, Char)]
// encrypt = String
// decrypt = String