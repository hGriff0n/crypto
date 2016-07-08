
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new Iden
        val msg = cipher.encrypt("Hello")

        var square = new Polybius(5, "jump")
        println("" + square.translate(2, 3))
        println("" + square.translate('N'))

        square.print
        println(msg)    // irk
        println(cipher.decrypt(msg)) // rip
    }
}

// TODO: Get Polybius Square working
// TODO: Get makefile/etc. working