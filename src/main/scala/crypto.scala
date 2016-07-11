
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new ADFGVX("PRIVACY")
        val msg = cipher.encrypt("ATTACK at 1200am")

        //val cipher = new ADFGX("CARGO")
        //val msg = cipher.encrypt("Attack at once")

        println(msg)
        println(msg == "DGDDDAGDDGAFADDFDADVDVFAADVX")
        //println(msg == "FAXDFADDDGDGFFFAFAXAFAFX")
        println(cipher.decrypt(msg))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file