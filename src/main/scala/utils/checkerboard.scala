package crypto.utils

object Checkerboard {}

// 10 x 3 "square"
//   0 1 2 3 4 5 6 7 8 9
//   E T   A O N   R I S        <- Randomly place ESTONIAR (high-frequency letters) leaving two spaces blank, no number is assigned to this row
// 2 B C D F G H J K L M        <- Label the next two rows with the numbers of the two skipped columns from the last step
// 6 P Q / U V W X Y Z .        <- Fill with the remaining alphabet, plus '.' and '/'. '/' is a escape character for numbers

// Translation: Letter is replaced by {row}{col}, with row = "" for letters in the first row (I'm using -1 for row)
// There's another thing about encipher the translated text through transposition/substition, but that can be applied afterwards

class Checkerboard(key: String) extends Translator {
    private val square = Array.ofDim[Char](3, 10)
    private var rows = Vector(-1)
    
    init()

    private def init() = {
        val rowstr = key.iterator

        for ((row, i) <- square.iterator.zipWithIndex; (col, j) <- row.iterator.zipWithIndex) {
            square(i)(j) = rowstr.next

            if (square(i)(j) == ' ') rows = rows :+ j
        }
    }

    def translate(c: Char): (Int, Int) = {
        for ((row, i) <- square.iterator.zipWithIndex; (col, j) <- row.iterator.zipWithIndex)
            if (square(i)(j) == c)
                return (rows(i), j)

        (-1, -1)
    }

    def translate(row: Int, col: Int) = square(rows.indexOf(row))(col)
    
    def this() = this(shuffled("AEINORST  ") + shuffled("BCDFGHJKLMPQUVWXYZ./"))
}