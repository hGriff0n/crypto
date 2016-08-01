
package crypto

import classical._;
import utils._;
import breeze.linalg._;

object demo {
    def main(args: Array[String]): Unit = {
        // VIC: ATTACK AT DAWN => ANWHRSANROAEER

        val cipher = new Beaufort("FORTIFICATION")
        val msg = cipher.encrypt("Defend the east wall of the castle")

        println(msg)
        println(msg == "LXFOPVEFRNHR")
        println(cipher.decrypt(msg))

        val tst: List[Int] = List(7, 4, 3, 6)
        val res = chainAdd(tst, 6)
        println(res)            // 7, 4, 3, 6, 1, 7

        val ret = sequentialize(tst)
        println(ret)            // 3, 1, 0, 2

    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file
    // Add "set offline := true" to avoid attempting to download from the internet