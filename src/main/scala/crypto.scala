
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new Nihilist("Russian")
        val msg = cipher.encrypt("DYNAMITE WINTER PALACE")

        println(msg)
        println(cipher.decrypt(msg))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file