
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {
        val cipher = new Affine(5, 3)
        val msg = cipher.encrypt("Abcde")

        println(msg)
        println(cipher.decrypt(msg))

        val err = new Affine(2, 5)
    }
}