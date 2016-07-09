
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new Playfair("Hello")
        val msg = cipher.encrypt("Bobby")

        println(msg)    // irk
        println(cipher.decrypt(msg)) // rip
    }
}

// TODO: Get Polybius Square working
// TODO: Get makefile/etc. working