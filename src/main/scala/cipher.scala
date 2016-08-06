package crypto;

import utils.CipherString;

trait Cipher {
    protected val lc = 'a' to 'z'
    protected val uc = 'A' to 'Z'

    def encrypt(msg: String): String
    def decrypt(msg: String): String = encrypt(msg).plaintext
}

class Iden extends Cipher {
    def encrypt(msg: String) = msg.ciphertext
}
