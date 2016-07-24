package crypto.classical

import crypto.Cipher;
import crypto.utils.{dvorak, qwerty};

class Substitution(from: String, to: String) extends Cipher {
    def encrypt(msg: String) = for (c <- msg) yield to(from.indexOf(c))
    def decrypt(msg: String) = for (c <- msg) yield from(to.indexOf(c))
}

class Dvorak extends Substitution(dvorak, qwerty)
class Qwerty extends Substitution(qwerty, dvorak)