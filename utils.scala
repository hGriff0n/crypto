package crypto

// Package object allows "free functions" to be used int library by importing crypto.utils._
package object utils {
    // Perform the extended euclidean algorithm to find gcd(a, b)
        // Returns a tuple (min(a, b), inverse, coprime)
    def euclid(a: Int, b: Int): (Int, Int, Boolean) = {
        //@tailrec
        def aux(a: Int, b: Int, x: Int, y: Int, xl: Int, yl: Int): (Int, Int) = {
            if (a == 0) return (xl, yl)
            val (q, r) = (b / a, b % a)
            aux(r, a, xl - q * x, yl - q * y, x, y)
        }

        // The variables' names assume that a < b
        val (inv, co) = aux(a, b, 1, 0, 0, 1)
        if (a > b) return (b, co, inv == 1)
        (a, inv, co == 1)
    }
    
    def modInv(a: Int, m: Int): Option[Int] = {
        val (_, inv, coprime) = euclid(a, m)
        if (coprime) return Some((inv + m) % m)
        None
    }
 
}