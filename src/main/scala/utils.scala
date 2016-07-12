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
 
    val everyTwoCharacters = "(?<=\\G.{2})"
}