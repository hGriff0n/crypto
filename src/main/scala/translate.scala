package crypto.utils

// TODO: Genericize the return types to mutliple dimensions
trait Translator {
    def translate(c: Char): (Int, Int)
    def translate(row: Int, col: Int): Char
    def translate(t: (Int, Int)): Char = translate(t._1, t._2)
}