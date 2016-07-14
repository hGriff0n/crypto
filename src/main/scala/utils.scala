package crypto

import scala.annotation.tailrec;

// Package object allows "free functions" to be used int library by importing crypto.utils._
package object utils {
    // Perform the extended euclidean algorithm to find gcd(a, b)
        // Returns a tuple (x=min(a, b), x^-1(mod 26), coprime(a, b))
    def euclid(a: Int, b: Int): (Int, Int, Boolean) = {
        @tailrec
        def aux(a: Int, b: Int, x: Int, y: Int, xl: Int, yl: Int): (Int, Int) = {
            if (a == 0) return (xl, yl)
            val (q, r) = (b / a, b % a)
            aux(r, a, xl - q * x, yl - q * y, x, y)
        }

        // The names assume that a < b
        val (inv, co) = aux(a, b, 1, 0, 0, 1)
        if (a > b) (b, co, inv == 1)
        else       (a, inv, co == 1)
    }
    
    def modInv(a: Int, m: Int): Option[Int] = {
        val (_, inv, coprime) = euclid(a, m)
        if (coprime) Some((inv + m) % m)
        else         None
    }

    def tabula(c: Char, sh: Int) = ((c.toInt - 39 + sh) % 26 + 65).toChar
    def tabula(a: Char, b: Char): Char = tabula(a, b.toInt - 65)
    def tabulaSub(c: Char, sh: Int): Char = tabula(c, -sh)
    def tabulaSub(a: Char, b: Char): Char = tabulaSub(a, b.toInt - 65)

    def shuffled(str: String) =
        scala.util.Random.shuffle(str.toList).mkString

    def mixed(incl_num: Boolean) = 
        shuffled(if (incl_num) "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" else "ABCDEFGHIKLMNOPQRSTUVWXYZ")

    // This has a feature warning but I don't know what
    def columnTranspose[A, C[A] <: Seq[A]](msg: C[A], num_col: Int) =
        msg.zipWithIndex                                                // Add the indices to the list
            .map(a => (a._1, a._2 % num_col))                           // So that I can group them into the columns
            .sortWith((a, b) => a._2 < b._2)                            // Sort the list so that items in the same column are next to each other
            .map(_._1)                                                  // Remove the indices from the list
            .grouped(math.ceil(msg.length / num_col.toFloat).toInt)	    // And then group by column
 
    val everyTwoCharacters = "(?<=\\G.{2})"
}