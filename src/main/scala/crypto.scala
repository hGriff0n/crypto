
package crypto

import classical._;
import utils._;

object demo {
    def main(args: Array[String]): Unit = {
        val board = new Checkerboard

        val o = board.translate('O')
        println(o)
        println(board.translate(o))

        val (row, col) = board.translate('M')
        println(s"($row,$col)")
        println(board.translate(row, col))

        // VIC: ATTACK AT DAWN => ANWHRSANROAEER
        // Trifid: Treaty ends Boer War. => MUAFN.EQRKREUTXQBW
    }
}

// Note: Enter `sbt ~run` in the terminal to have scala run the project after saving a file