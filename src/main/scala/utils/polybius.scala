package crypto.utils

object Polybius {
    // "static" method to enable implementing `this(Int, String)` in terms of `this(Int, Fn)`
    private def genKeyFn(key: String, s: Int): (Int, Int, Int) => Char = {
        val tmp = (if (s == 5) key.replace('J', 'I').replace('j', 'I') else key).distinct.toUpperCase diff ", "                 // Replace j's with i's if the square can't hold all letters
        val actkey = tmp + ((if (s == 5) "ABCDEFGHIKLMNOPQRSTUVWXYZ" else "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890") diff tmp)

        def keyFn(iter: Int, x: Int, y: Int) = iter match {
            case iter if (iter < actkey.length) => actkey(iter)
            case iter => ' '
        }

        keyFn
    }
}


class Polybius private (s: Int) extends Translator {
    if (s < 5 || s > 6) throw new Exception(s"Polybius($s): s must be between 5 and 6")
    private var square: Array[Array[Char]] = Array.ofDim[Char](s, s)

    // Construct the square based on a given rule function
    def this(siz: Int, fn: (Int, Int, Int) => Char) {
        this(siz)

        var iter = 0
        for ((row, i) <- square.iterator.zipWithIndex; (col, j) <- row.iterator.zipWithIndex) {
            square(i)(j) = fn(iter, i, j)
            iter += 1
        }     
    }
    def this(siz: Int, key: String) = this(siz, Polybius.genKeyFn(key, siz))

    // Get the character at the given position
    def translate(x: Int, y: Int) = square(x)(y)

    // Get the position of the given character
        // Note: Not the most efficient implementation for encoding (could be memoized), but the losses are negligable
        // and adding a map to handle translate(Char) would complicate set and fill more than is acceptable ATM
    def translate(c: Char) =
        (for {
            (row, i) <- square.iterator.zipWithIndex
            (ch, j) <- row.iterator.zipWithIndex
            if (ch == c)
        } yield i -> j).next

    // Debug method for printing the square
    def print() = println(square.deep.mkString("\n"))
}