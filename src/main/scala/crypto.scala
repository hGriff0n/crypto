
package crypto

import classical._;
import utils._;
import breeze.linalg._;

object demo {
    def main(args: Array[String]): Unit = {
        // VIC: ATTACK AT DAWN => ANWHRSANROAEER

        val cipher = new Hill(3, "GYBNQKURP")
        val msg = cipher.encrypt("ACTA")

        println(msg)
        println(msg == "POH")
        println(cipher.decrypt(msg))
        //println(Hill.stringify(t))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file