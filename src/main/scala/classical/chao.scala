package crypto.classical;

import crypto.utils.Polybius;

// This is similar to VIC
class ChaoCipher extends crypto.Cipher {
    override def encrypt(msg: String) = msg
    override def decrypt(msg: String) = msg
}