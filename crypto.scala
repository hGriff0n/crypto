
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new Atbash
        val msg = cipher.encrypt("rip")

        println(msg)    // irk
        println(cipher.decrypt(msg)) // rip
    }
}