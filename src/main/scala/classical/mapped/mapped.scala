package crypto.classical.mapped

import crypto.utils.{tabula, tabulaSub, CipherString, modInv, dvorak, qwerty}

trait Cipher[T] extends crypto.Cipher {
    protected var encMap: Map[Char, T] = Map()
    protected var decMap: Map[T, Char] = Map()

    def mapping = List(encMap, decMap)

    def mapenc(msg: List[Char]) = for (c <- msg) yield (c, encMap(c))
    def mapdec(msg: List[T]) = for (c <- msg) yield (c, decMap(c))

    override def encrypt(msg: String) = mapenc(msg.ciphertext.toList).map(_._2).mkString
    override def decrypt(msg: String) = mapdec(msg.map(_.asInstanceOf[T]).toList).map(_._2).mkString.plaintext
}

class Caeser(shft: Int) extends Cipher[Char] {
    encMap = uc.map(c => c -> tabula(c, shft)).toMap
    decMap = uc.map(c => c -> tabulaSub(c, shft)).toMap
}

class Affine(a: Int, b: Int) extends Cipher[Char] {
    private val inv: Int = modInv(a, 26) match {
        case Some(num) => num
        case None => throw new Exception(s"$a and 26 are not coprime")
    }

    encMap = uc.map(c => c -> encode(c, a, b).toChar).toMap
    decMap = uc.map(c => c -> encode(c, inv, inv * -b % 26).toChar).toMap

    private def encode(c: Char, a: Int, b: Int) = (((c.toInt - 65) * a + b + 26) % 26) + 'A'
}

class Substitution(from: String, to: String) extends Cipher[Char] {
    encMap = from.ciphertext.map(c => c -> to(from.indexOf(c))).toMap
    decMap = to.ciphertext.map(c => c -> from(to.indexOf(c))).toMap
}

class Rot13 extends Caeser(13)
class Atbash extends Affine(25, 25)
class Dvorak extends Substitution(dvorak, qwerty)
class Qwerty extends Substitution(qwerty, dvorak)
class Keyword(key: String) extends Substitution("ABCDEFGHIJKLMNOPQRSTUVWXYZ", key + (key diff "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
