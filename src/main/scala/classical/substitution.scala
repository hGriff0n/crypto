package crypto.classical

import crypto.utils.{dvorak, qwerty};
import crypto.utils.CipherString;

class Substitution(from: String, to: String) extends BasicCipher {
    encMap = from.ciphertext.map(c => c -> to(from.indexOf(c))).toMap
    decMap = to.ciphertext.map(c => c -> from(to.indexOf(c))).toMap
}

class Dvorak extends Substitution(dvorak, qwerty)
class Qwerty extends Substitution(qwerty, dvorak)

class Keyword(key: String) extends Substitution("ABCDEFGHIJKLMNOPQRSTUVWXYZ", key + (key diff "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))