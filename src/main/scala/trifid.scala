package crypto.classical;

import crypto.Cipher;
import crypto.utils.{Polybius, shuffled};

// A cubic version of a polybius square is created from a mixed alphabet (3x3x3)
    // Really just three tables with 9 letters in each
    // Commonly a map of character => trifid coord is made
// The coordinates for each character are written out in columns and then read in rows
// Before being divided into triplets and translated back through the table

// TODO: Move ADFGVX.columnTranspose into utils (and make it generic)
class Trifid extends Cipher {
    private val table = scala.collection.mutable.Map[Char, List[Int]]()

    init()

    private def init() = {
        var iter = 0
        for (c <- shuffled("ABCDEFGHIJKLMNOPQRSTUVWXYZ.")) {
            table(c) = List(iter / 9, (iter % 9) / 3, (iter % 9) % 3)
            iter += 1
        }
    }

    private def columnTranspose(msg: List[Int], num_col: Int) =
        msg.zipWithIndex                                                // Add the indices to the list
            .map(a => (a._1, a._2 % num_col))                           // So that I can group them into the columns
            .sortWith((a, b) => a._2 < b._2)                            // Sort the list so that items in the same column are next to each other
            .map(_._1)                                                  // Remove the indices from the list
            .grouped(math.ceil(msg.length / num_col.toFloat).toInt)	    // And then group by column

    println(table.mkString("\n"))

    // Find character that matches the list
    private def charFor(trip: List[Int]): Char = table.find(_._2 == trip).getOrElse(('~', List()))._1

    def encrypt(msg: String) = {
        val trips = for (c <- msg.toUpperCase.replaceAll(" ", "")) yield table(c)
        columnTranspose(trips.toList.flatten, 3)
            .map(l => 
                (for (trip <- l.sliding(3, 3)) yield charFor(trip.toList)).mkString
            ).mkString
    }

    def decrypt(msg: String) = {
        val trips = for (c <- msg.toUpperCase.replaceAll(" ", "")) yield table(c)
        columnTranspose(trips.toList.flatten, msg.length)
            .map(l =>
                (for (trip <- l.sliding(3, 3)) yield charFor(trip.toList)).mkString
            ).mkString
    }
}