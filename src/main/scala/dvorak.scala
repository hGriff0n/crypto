package crypto.classical

import crypto.Cipher;

object Dvorak {
    protected val dvorak = "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
    protected val qwerty = "`1234567890[]',.pyfgcrl/=\\aoeuidhtns-;qjkxbmwvz~!@#$%^&*(){}\"<>PYFGCRL?+|AOEUIDHTNS_:QJKXBMWVZ"
}

class Dvorak(to: Boolean) extends Cipher {
    private val enc = if (to) Dvorak.dvorak else Dvorak.qwerty
    private val dec = if (to) Dvorak.qwerty else Dvorak.dvorak
    
    def this() = this(true)

    def encrypt(msg: String) = for (c <- msg) yield enc(dec.indexOf(c))
    def decrypt(msg: String) = for (c <- msg) yield dec(dec.indexOf(c))
}

class Qwerty extends Dvorak(false)