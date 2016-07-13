package crypto.classical;

import crypto.Cipher;
import crypto.utils.{Polybius, mixed};

class ADFGVX(key: String, adfgx: Boolean) extends Cipher {
    private val sq = new Polybius(if (adfgx) 5 else 6, mixed(!adfgx))

    private val encKey = key.zipWithIndex.sortWith((a, b) => a._1 < b._1).map(_._2).toList
    private val decKey = encKey.zipWithIndex.sortWith((a, b) => a._1 < b._1).map(_._2).toList

    private def columnTranspose(msg: List[Char], num_col: Int) =
        msg.zipWithIndex                                                // Add the indices to the list
            .map(a => (a._1, a._2 % num_col))                           // So that I can group them into the columns
            .sortWith((a, b) => a._2 < b._2)                            // Sort the list so that items in the same column are next to each other
            .map(_._1)                                                  // Remove the indices from the list
            .grouped(math.ceil(msg.length / num_col.toFloat).toInt)	    // And then group by column

    def this(key: String) = this(key, false)

    def encrypt(msg: String) = {
        // Translate the message into it's flattened polybius coordinates
        val coords = (for (c <- msg.toUpperCase.replaceAll(" ", "")) yield sq.translate(c)).toList
            .flatMap(t => List(t._1, t._2))         // Flatten the resulting tuples
            .map(_ match {                          // Translate coordinates into ADFG(V)X
                case 0 => 'A'
                case 1 => 'D'
                case 2 => 'F'
                case 3 => 'G'
                case 4 => if (adfgx) 'X' else 'V'
                case 5 => 'X'
            })

        // Apply a columnar transposition on the message
        val cols = columnTranspose(coords, key.length).toList
        
        // Read off the columns in the sorted order
        encKey.map(cols(_).mkString).mkString
    }

    def decrypt(msg: String) = {
        val num_over = msg.length % key.length
        val num_take = msg.length / key.length.toFloat
        val max = math.ceil(num_take).toInt
        val min = math.floor(num_take).toInt

        // Divide the message into the columns
            // Note: Since the columns are shuffled in encryption, a simple grouping wouldn't be correct
        var iter = msg.iterator
        val cols = encKey.map(a => {
            val num_items = if (a < num_over) max else min
            val ret = iter.take(num_items)
            iter = iter.drop(num_items)         // Update the iterator
            ret.toList
        }).toList

        // Put the columns back into the correct ordering
        val orig = columnTranspose(decKey.flatMap(cols(_)).toList, max).toList
            .flatMap(_.map(_ match {                    // Undo the ADFG(V)X translations
                case 'A' => 0
                case 'D' => 1
                case 'F' => 2
                case 'G' => 3
                case 'V' => 4
                case 'X' => if (adfgx) 4 else 5
            })).toList

        // Translate the coordinates back to the message
        (for (co <- orig.sliding(2, 2)) yield sq.translate(co(0), co(1))).mkString
    }
}

class ADFGX(key: String) extends ADFGVX(key, true)