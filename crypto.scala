
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new Rot13
        val msg = cipher.encrypt("Why did the chicken cross the road?\nGb trg gb gur bgure fvqr!")

        println(msg)    // irk
        println(cipher.decrypt(msg)) // rip
    }
}