
package crypto

import classical._;
import classical.mapped._;
import utils._;
//import spire.algebra._;   // provides algebraic type classes
//import spire.math._;      // provides functions, types, and type classes
//import spire.implicits._; // provides infix operators, instances and conversions

object demo {
    def main(args: Array[String]): Unit = {
        // VIC: ATTACK AT DAWN => ANWHRSANROAEER

        val cipher = new Caeser(5)
        
        // "This is a plaintext message" causes errors (length !/ 3)
        val msg = cipher.encrypt("how are you")
        println(msg)
        println(cipher.decrypt(msg))

        println(cipher.about)
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file
    // Add "set offline := true" to avoid attempting to download from the internet