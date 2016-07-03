package crypto.classical

class Caeser(val shft: Int) {
    private val lc = 'a' to 'z'
    private val uc = 'A' to 'Z'

    private def encode(c: Char, sh: Int): Char = c match {
        case c if lc.contains(c) =>
        	((c.toInt - 71 + sh) % 26 + 97).toChar
        case c if uc.contains(c) =>
        	((c.toInt - 39 + sh) % 26 + 65).toChar
        case c => c
    }

    def encrypt(msg: String): String = 
        for (c <- msg) yield encode(c, shft)

    def decrypt(msg: String): String = 
        for (c <- msg) yield encode(c, -shft)
}