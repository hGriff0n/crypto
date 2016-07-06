
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new Iden
        val msg = cipher.encrypt("Hello")

        println(msg)    // irk
        println(cipher.decrypt(msg)) // rip
    }
}