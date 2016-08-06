package crypto.classical;

import crypto.utils.CipherString;

trait BasicCipher extends crypto.Cipher {
    protected var encMap: Map[Char, Char] = Map()
    protected var decMap: Map[Char, Char] = Map()

    def mapping = List(encMap, decMap)

    def mapenc(msg: String) = for (c <- msg) yield (c, encMap(c))
    def mapdec(msg: String) = for (c <- msg) yield (c, decMap(c))

    override def encrypt(msg: String) = mapenc(msg.ciphertext).map(_._2).mkString
    override def decrypt(msg: String) = mapdec(msg).map(_._2).mkString.plaintext
}