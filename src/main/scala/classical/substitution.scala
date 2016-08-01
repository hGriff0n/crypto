package crypto.classical

import crypto.Cipher;
import crypto.utils.{dvorak, qwerty};

class Substitution(from: String, to: String) extends Cipher {
    override def encrypt(msg: String) = for (c <- msg) yield to(from.indexOf(c))
    override def decrypt(msg: String) = for (c <- msg) yield from(to.indexOf(c))
}

class Dvorak extends Substitution(dvorak, qwerty)
class Qwerty extends Substitution(qwerty, dvorak)

class Keyword(key: String) extends Substitution("ABCDEFGHIJKLMNOPQRSTUVWXYZ", key + (key diff "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
    override def encrypt(msg: String) = super.encrypt(msg.toUpperCase.replaceAll(" ", ""))
} 