
package crypto

import classical._;
import utils._;
//import spire.algebra._;   // provides algebraic type classes
//import spire.math._;      // provides functions, types, and type classes
//import spire.implicits._; // provides infix operators, instances and conversions

object demo {
    def main(args: Array[String]): Unit = {
        // VIC: ATTACK AT DAWN => ANWHRSANROAEER

        val (mi, k1, k2, c) = VIC.interKeys("all the people are dead but I'm gonna keep dancing", 391752, 15)
        val c0 = VIC.checker("ASSIGNED OBJECTIVES INVALIDATED . REPORT IMMEDIATELY TO SAFE HOUSE . AWAIT EXTRACTION INSTRUCTIONS WITHIN WEEK", c)
        val c1 = VIC.firstTranspose(k1, c0)
        val c2 = VIC.secondTranspose(k2, c1)
        val ct = VIC.finalize(mi, 391752, c2)
        println(ct)

        val cipher = new VIC("all the people are dead but I'm gonna keep dancing", 391752, 15)
        val msg = cipher.encrypt("ASSIGNED OBJECTIVES INVALIDATED . REPORT IMMEDIATELY TO SAFE HOUSE . AWAIT EXTRACTION INSTRUCTIONS WITHIN WEEK")

        println(msg)
        println(cipher.decrypt(msg))
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file
    // Add "set offline := true" to avoid attempting to download from the internet