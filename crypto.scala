
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {        
        val cipher = new Bifid(5)
        val msg = cipher.encrypt("FLEEATONCE")

        println(msg)
        println(cipher.decrypt(msg))
    }
}

// TODO: Get makefile/etc. working