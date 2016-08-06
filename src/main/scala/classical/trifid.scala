package crypto.classical;

import crypto.utils.{Polybius, shuffled, columnTranspose};

// A cubic version of a polybius square is created from a mixed alphabet (3x3x3)
    // Really just three tables with 9 letters in each
    // Commonly a map of character => trifid coord is made
// The coordinates for each character are written out in columns and then read in rows
// Before being divided into triplets and translated back through the table

class Trifid extends crypto.Cipher {
    private val table = scala.collection.mutable.Map[Char, List[Int]]()

    // Populate the table with the character->coordinate mappings
    for ((c, iter) <- shuffled("ABCDEFGHIJKLMNOPQRSTUVWXYZ.").zipWithIndex)
        table(c) = List(iter / 9, (iter % 9) / 3, (iter % 9) % 3)

    private def translate(msg: String, group: Int) = {
        val trips = for (c <- msg.toUpperCase.replaceAll(" ", "")) yield table(c)
        columnTranspose(trips.toList.flatten, group)
            .map(l => 
                (for (trip <- l.sliding(3, 3))                      // For every coordinate triplet
                    yield table.find(_._2 == trip).get._1)          // Find the character that's mapped to it
                .mkString
            ).mkString
    }

    override def encrypt(msg: String) = translate(msg, 3)
    override def decrypt(msg: String) = translate(msg, msg.length)
}