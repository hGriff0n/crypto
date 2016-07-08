package crypto;

trait Cipher {
    protected val lc = 'a' to 'z'
    protected val uc = 'A' to 'Z'

    def encrypt(msg: String): String
    def decrypt(msg: String): String
}

class Iden extends Cipher {
    def encrypt(msg: String) = msg
    def decrypt(msg: String) = msg
}
