
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
        //println(Hill.stringify(t))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file
    // Add "set offline := true" to avoid attempting to download from the internet