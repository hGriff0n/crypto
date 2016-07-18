package crypto.classical;

import crypto.Cipher;
import crypto.utils.modInv;
import breeze.linalg._;
import breeze.numerics._;

// TODO: Improve organization
object Hill {
    protected def makeInvertible(n: Int) = {
        DenseMatrix.zeros[Int](n, n)
    }

    protected def makeMatrix(str: String, n: Int) = {
        DenseMatrix.zeros[Int](n, n)
    }

    // TODO: Genericize on n
    protected def makeVector(str: String) = {
        DenseVector((str(0) - 65) % 26, (str(1) - 65) % 26, (str(2) - 65) % 26)
    }

    // Converts the DenseVector (mod 26) to a vector of the mapped characters
    def stringify(vec: DenseVector[Int]) = vec.map(i => ((i % 26) + 65).toChar).toScalaVector

    // TODO: Figure out what to do if str.length % n != 0
    def split(str: String, n: Int) = str.grouped(n).map(makeVector(_))

}

// Each number is represented by a number mod 26
// Encrypt: Multiply each block of n numbers by a n.n matrix
// Decrypt: Multiply each block of n numbers by the inverse of the matrix
// Matrix must have an inverse and gcd(det(M), 26) = 1
class Hill private (m: DenseMatrix[Int], n: Int) extends Cipher {
    private val inv = modInv(m, 26)
    // Note: The primary constructor is only private because breeze constructs matrices in column-major order and
        // all my test data is in row-major order. I'm not sure yet which is the more usable option so I'm sticking with this

    def this(m: DenseMatrix[Int]) = this(m.t, m.rows)
    def this(n: Int) = this(Hill.makeInvertible(n), n)
    def this(n: Int, key: String) = this(Hill.makeMatrix(key, n))

    def encrypt(msg: String) = Hill.split(msg, n).flatMap(v => Hill.stringify(m.t * v)).mkString
    def decrypt(msg: String) = Hill.split(msg, n).flatMap(v => Hill.stringify(inv.t * v)).mkString
}