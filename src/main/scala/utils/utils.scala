package crypto;

import scala.annotation.tailrec;
import breeze.linalg.{det, inv, DenseMatrix};

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
    
    // TODO: modInv(441, 26) => None (Should give Some(25))
    def modInv(a: Int, m: Int): Option[Int] = {
        val (t, inv, coprime) = euclid(a, m)
        if (coprime) Some((inv + m) % m)
        else         None
    }

    def tabula(c: Char, sh: Int) = ((c.toInt - 39 + sh) % 26 + 65).toChar
    def tabula(a: Char, b: Char): Char = tabula(a, b.toInt - 65)
    def tabulaSub(c: Char, sh: Int): Char = tabula(c, -sh)
    def tabulaSub(a: Char, b: Char): Char = tabulaSub(a, b.toInt - 65)

    def shuffled(str: String) = scala.util.Random.shuffle(str.toList).mkString
    def mixed(incl_num: Boolean) = shuffled(if (incl_num) "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" else "ABCDEFGHIKLMNOPQRSTUVWXYZ")


    // This has a feature warning but I don't know what
    def columnTranspose[A, C[A] <: Seq[A]](msg: C[A], num_col: Int) =
        msg.zipWithIndex                                                // Add the indices to the list
            .map(a => (a._1, a._2 % num_col))                           // So that I can group them into the columns
            .sortWith((a, b) => a._2 < b._2)                            // Sort the list so that items in the same column are next to each other
            .map(_._1)                                                  // Remove the indices from the list
            .grouped(math.ceil(msg.length / num_col.toFloat).toInt)	    // And then group by column

    // TODO: Change to a better implementation (http://stackoverflow.com/questions/4287721/easiest-way-to-perform-modular-matrix-inversion-with-python)
        // The current solution may have rounding issues
    def modInv(mat: DenseMatrix[Int], m: Int): DenseMatrix[Int] = {
        val d = det(mat).toInt
        val dinv = modInv(d % 26, 26).get     // For some reason modInv doesn't work if a > b

        inv(mat)
            .map(x => {
                val t = math.round(x * d).toInt * dinv
                ((t % 26) + 26) % 26
            })
    }

    // Perform chain addition (lagged fibonacci generation) to generate a List of size n
    def chainAdd(d: List[Int], n: Int, mod: Int = 10): List[Int] = {
        val add = d.sliding(2).map(if (mod > 0) (_.sum % mod) else _.sum).toList
        val ret = d ++ add

        (if (ret.size < n) {
            val orig = d.slice(0, (d.size - 1))
            orig ++ chainAdd(List(d.last) ++ add, n - orig.size, mod)
        } else ret).slice(0, n)
    }
    def chainAdd(d: List[Char], n: Int): List[Int] = chainAdd(d.map(_.toInt - 65), n, 26)
    def chainAdd(d: String, n: Int): List[Int] = chainAdd(d.toUpperCase.toList.map(_.toInt - 65), n, 26)


    // Transform d into a List of [0..N) representing their position in an ordered list
    def sequentialize[T <% Ordered[T]](d: List[T]) =
        d.zipWithIndex
         .sortWith((a, b) => a._1 < b._1)         // Sort the given list but maintain the original positioning
         .zipWithIndex                            // Add in the sequentialization
         .sortWith((a, b) => a._1._2 < b._1._2)   // Return to the original ordering
         .map(_._2 + 1)                           // Remove the original data

    // Split a number into its digits
    def digits[T](i: T)(implicit num: Numeric[T]): scala.collection.immutable.IndexedSeq[Int] = i.toString map(_.asDigit)
    def digits[T](i: T, n: Int)(implicit num: Numeric[T]): scala.collection.immutable.IndexedSeq[Int] = {
        val res = digits(i)(num)
        Vector.fill(n - res.length)(0) ++ res
    }

    val everyTwoCharacters = "(?<=\\G.{2})"
    val dvorak = "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
    val qwerty = "`1234567890[]',.pyfgcrl/=\\aoeuidhtns-;qjkxbmwvz~!@#$%^&*(){}\"<>PYFGCRL?+|AOEUIDHTNS_:QJKXBMWVZ"
}