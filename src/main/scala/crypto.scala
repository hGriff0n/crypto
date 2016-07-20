
package crypto

import classical._;
import utils._;
import breeze.linalg._;

object demo {
    def main(args: Array[String]): Unit = {
        // VIC: ATTACK AT DAWN => ANWHRSANROAEER

        val mat = DenseMatrix((6, 24, 1), (13, 16, 10), (20, 17, 15))

        val cipher = new Hill(mat)
        val msg = cipher.encrypt("ACTA")

        println(msg)
        println(msg == "POH")
        println(cipher.decrypt(msg))
        //println(Hill.stringify(t))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file