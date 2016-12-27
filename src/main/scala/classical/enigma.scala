package crypto.classical;

import crypto.utils.Polybius;

// http://practicalcryptography.com/ciphers/enigma-cipher/
class Enigma extends crypto.Cipher {
    override def encrypt(msg: String) = msg
    override def decrypt(msg: String) = msg
}