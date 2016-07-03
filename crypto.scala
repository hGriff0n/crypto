
package crypto

import classical._;

object demo {
    def main(args: Array[String]): Unit = {
        val cipher = new Caeser(5)
        val msg = cipher.encrypt("Zzzzz")

        println(msg)
        println(cipher.decrypt(msg))
    }
}