package crypto.classical.mapped

import crypto.utils.{tabula, tabulaSub, CipherString, modInv, dvorak, qwerty}

class MappedContext(encMap: Map[Char, Char], decMap: Map[Char, Char]) extends crypto.Context {
    def >>(msg: String) = for (c <- msg) yield encMap(c)
    def <<(msg: String) = for (c <- msg) yield decMap(c)

    def view = List(encMap, decMap).mkString("\n")

    def enc = encMap
    def dec = decMap
}

abstract class Cipher extends crypto.Cipher {
    protected var con: MappedContext = null
    protected def desc: String

    def context = con.view

    override def encrypt(msg: String) = con >> msg.ciphertext
    override def decrypt(msg: String) = (con << msg).plaintext

    override def about = {
        val plaintext = "How are you"
        val ciphertext = encrypt(plaintext)
        val description = desc

        val encstr = con.enc map { case (k, v) => k } mkString(" ")
        val decstr = con.enc map { case (k, v) => v } mkString(" ")

    s"""
    This cipher is one of (or inherits from) the earliest forms of cryptography
    Developed over the course of human history. As a consequence, these ciphers
    Put a premium on being able to performed by hand (And are consequently
    Almost trivially easy to crack with even the smallest amounts of info).
$description
    But while the core production rule of this cipher may be unique, it belongs
    To a family of classical ciphers that can be implicitly represented by a
    Simple table mapping the plaintext characters to the ciphertext character.
    Encryption in this family becomes a simple case of applying this table
    lookup for every character of the plaintext string in turn. Decryption can
    similarly be turned into a reverse lookup using the same tables.

        From:  $encstr
        -------------------------------------------------------------
          To:  $decstr

    So using our example plaintext of \"$plaintext\", we get this encryption:

        $plaintext => $ciphertext
    """
    }
}

class Caeser(shft: Int) extends Cipher {
    con = new MappedContext(uc.map(c => c -> tabula(c, shft)).toMap, uc.map(c => c -> tabulaSub(c, shft)).toMap)

    protected def desc =
    s"""
    The Caeser cipher is the oldest know cryptography scheme, commonly credited
    To Julius Caeser, hence the name, during his historic campaigns in Gaul
    (modern day France). This cipher is one of the most ubiquitous and well-known
    ciphers in history, you've probably used one a few times yourself for \"fun\".

    The cipher itself is a simple alphabetic shift ($shft for our example cipher).
    In a shift (or tabula) cipher, each character is converted into a number from
    0 to 25 (with A = 0). The shift amount is then added to each number (mod 26)
    And then converted back into character following the same transform rule. So
    Supposing that the shift character was A (shift of 0) encrypting the plaintext
    String would return the original message as the ciphertext.

    Decryption is performed by simplify applying the reverse transformation onto the
    ciphertext string (subtracting $shft characters in our case). It's also possible
    (through modular arithmetic) for another caeser cipher to encrypt the ciphertext
    and end up producing the original plaintext.
    """
}

class Affine(a: Int, b: Int) extends Cipher {
    private val inv: Int = modInv(a, 26) match {
        case Some(num) => num
        case None => throw new Exception(s"$a and 26 are not coprime")
    }

    con = new MappedContext(uc.map(c => c -> encode(c, a, b).toChar).toMap, uc.map(c => c -> encode(c, inv, inv * -b % 26).toChar).toMap)

    protected def desc =
    s"""
    This is an Affine cipher of the form %dx + %d (mod 26)
    """.format(a, b)

    private def encode(c: Char, a: Int, b: Int) = (((c.toInt - 65) * a + b + 26) % 26) + 'A'
}

class Substitution(from: String, to: String) extends Cipher {
    con = new MappedContext(from.ciphertext.map(c => c -> to(from.indexOf(c))).toMap, to.ciphertext.map(c => c -> from(to.indexOf(c))).toMap)

    protected def desc =
    s"""
    This is a substitution cipher from $from to $to
    """
}

class Rot13 extends Caeser(13) {
    override protected def desc =
    s"""
    A special case of a Caeser cipher, with
    """
}
class Atbash extends Affine(25, 25) {
    override protected def desc =
    s"""
    A special case of an Affine cipher (with a,b = 25)
    """
}
class Dvorak extends Substitution(dvorak, qwerty) {
    override protected def desc =
    s"""
    Done more by accident than with any desire for protection
    """
}

class Qwerty extends Substitution(qwerty, dvorak)
class Keyword(key: String) extends Substitution("ABCDEFGHIJKLMNOPQRSTUVWXYZ", key + (key diff "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
