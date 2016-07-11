
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new ADFGVX("PRIVACY")
        val msg = cipher.encrypt("ATTACK at 1200am")

        println(msg)        // DGDDDAGDDGAFADDFDADVDVFAADVX
        println(cipher.decrypt(msg))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file