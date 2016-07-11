package crypto.classical;

import crypto.Cipher;
import crypto.utils.Polybius;

class Nihilist(key: String) extends Cipher {
    private val sq = new Polybius(5, mixed(false))
    private val cKey =
        key.toUpperCase.map(c => {
            val (t, o) = sq.translate(c)
            t * 10 + o
        }).toList

    def encrypt(msg: String) = {
        var iter = 0
        msg.toUpperCase.replaceAll(" ", "")
            .map(c => {
                val (tens, ones) = sq.translate(c)
                val tmp = cKey(iter)
                iter = (iter + 1) % cKey.size
                tens * 10 + ones + tmp
            }).mkString(" ")
    }
    
    def decrypt(msg: String) = {
        var iter = 0
        msg.split(" ")
            .map(s => {
                val num = s.toInt - cKey(iter)
                iter = (iter + 1) % cKey.size
                val tens: Int = num / 10
                sq.translate(tens, num - (tens * 10))
            }).mkString
    }
}
