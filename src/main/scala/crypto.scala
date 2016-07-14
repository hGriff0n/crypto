
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {
        // VIC: ATTACK AT DAWN => ANWHRSANROAEER
        // Trifid: Treaty ends Boer War. => MUAFN.EQRKREUTXQBW

        val cipher = new Autokey("QUEENLY")
        val msg = cipher.encrypt("ATTACK AT ONCE")

        println(msg)
        println(msg == "MUAFN.EQRKREUTXQBW")        // This won't give true cause I'm shuffling the alphabet
        println(cipher.decrypt(msg))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file