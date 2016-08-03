
package crypto

import classical._;
import utils._;
//import spire.algebra._;   // provides algebraic type classes
//import spire.math._;      // provides functions, types, and type classes
//import spire.implicits._; // provides infix operators, instances and conversions

object demo {
    def main(args: Array[String]): Unit = {
        // VIC: ATTACK AT DAWN => ANWHRSANROAEER

        val cipher = new Beaufort("FORTIFICATION")
        val msg = cipher.encrypt("Defend the east wall of the castle")

        println(msg)
        println(msg == "LXFOPVEFRNHR")
        println(cipher.decrypt(msg))

        val (mi, k1, k2, c) = VIC.interKeys("all the people are dead but I'm gonna keep dancing", 391752, 15)
        println(VIC.checker("ASSIGNED OBJECTIVES INVALIDATED . REPORT IMMEDIATELY TO SAFE HOUSE . AWAIT EXTRACTION INSTRUCTIONS WITHIN WEEK", c))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file
    // Add "set offline := true" to avoid attempting to download from the internet