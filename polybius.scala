package crypto.utils

object Polybius {
    private def genKeyFn(key: String): (Int, Int, Int) => Char = {
        val actkey = key diff " ,J"                             // Handle I/J split for now (not the best solution)
        val rem = "ABCDEFGHIKLMNOPQRSTUVWXYZ" diff key           // by removing J from the set

        def keyFn(iter: Int, x: Int, y: Int) = iter match {
            case iter if (iter < actkey.length) => actkey(iter)
            case iter => rem(iter - actkey.length)
        }

        keyFn
    }
}


// TODO: Find a better way of handling I/J in keys
// TODO: Restrict square to 5 or 6 (and allow digits in 6)
class Polybius private (s: Int) {
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

    // Construct the square from the given key
    def this(siz: Int, key: String) = this(siz, Polybius.genKeyFn(key.distinct.toUpperCase))

    // Get the character at the given position
    def translate(x: Int, y: Int) = square(x)(y)
    def translate(t: (Int, Int)): Char = translate(t._1, t._2)

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