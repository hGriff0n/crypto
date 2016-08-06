package crypto.classical;

import crypto.utils.modInv;
import breeze.linalg._;
import breeze.numerics._;

object Hill {
    // Turn a string into a matrix for the Hill cipher
    protected def makeMatrix(str: String, n: Int) = (new DenseMatrix(n, n, str.map(c => (c - 65) % 26).toArray)).t

    // Convert the string to it's vector representation
    protected def makeVector(str: String) = DenseVector(str.map(c => (c - 65) % 26).toArray)

    // Converts the DenseVector (mod 26) to a vector of the mapped characters
    protected def stringify(vec: DenseVector[Int]) = vec.map(i => ((i % 26) + 65).toChar).toScalaVector

    // Group the string into encryption blocks
    protected def split(str: String, n: Int): Iterator[DenseVector[Int]] = str.length match {
        case l if (l % n != 0) => split(str + ("X" * (n - l % n)), n)
        case l => str.grouped(n).map(makeVector(_))
    }

}

// Each letter is represented by a number mod 26
// Encrypt: Multiply each block of n numbers by a n.n matrix
// Decrypt: Multiply each block of n numbers by the inverse of the matrix
// Matrix must have an inverse and gcd(det(M), 26) = 1
class Hill private (m: DenseMatrix[Int], n: Int) extends crypto.Cipher {
    private val inv = modInv(m, 26)

    // Note: I store the transpose of the matrix because breeze constructs matrices in column-major order and all my
            // test data is in row-major order. I'm not sure yet which is the more usable option so I'm sticking with this

    def this(m: DenseMatrix[Int]) = this(m.t, m.rows)
    def this(n: Int, key: String) = this(Hill.makeMatrix(key, n))

    // For some reason, I can't combine these into one function
    override def encrypt(msg: String) = Hill.split(msg.toUpperCase.replaceAll(" ", ""), n).flatMap(v => Hill.stringify(m.t * v)).mkString
    override def decrypt(msg: String) = Hill.split(msg, n).flatMap(v => Hill.stringify(inv.t * v)).mkString
}