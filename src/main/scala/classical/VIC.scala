package crypto.classical;

import crypto.Cipher;
import crypto.utils.{sequentialize, chainAdd, digits, columnTranspose};
// Procedure from http://everything2.com/user/raincomplex/writeups/VIC+cipher

// TODO: Figure out how to decrypt

/**
Final Production:
    Given:
        MI => 5 choosen digits produced for the Intermediate Keys
        D => Date represented as a digit
        P => Enciphered text produced by the second transposition
    Produces:
        C => Final ciphertext
    Procedure:
        Split P into groups of 5 digits => G
        Insert MI into G last(D) groups from the end => C
            last(D) == 1 => MI is the last group of C
*/

/**
Second Transposition:
    Given:
        K2 => Key produced from Intermediate Key step
        P => Plaintext encoded from the first transpostion
    Produces:
        C => Reordered plaintext
    Procedure:
        let size(P) / size(K2) => H
            size(P) % size(K2) => L
            indexof(1 to size(K2), K2) => I
            next(I) - 1 => R
        Construct a table, dim[size(K2), H] => T
        For N from 0 to H - 2
            if R == size(K2)
                let next(I) - 1 => R
            take next R characters of P => K
            fill the first R spots of T[N] with K => T
        Fill the first L spots of T[H - 1] with the next L characters of P => T
        Fill the open spots of T with the remaining characters of P => T
        Read T column wise in the order from K2 => C
            Column mapped to '1' in K1 is read first, etc..
*/

/**
First Transposition:
    Given:
        K1 => Key produced from Intermediate Key step
        P => Plaintext encoded by the straddling checkerboard
    Produces:
        C => Reordered plaintext according to K1
    Procedure:
        Write P as a table with size(K1) rows => T
        Read T column wise in the order from K1 => C
            Column mapped to '1' in K1 is read first, etc..
*/

/**
Straddling Checkerboard
    Given:
        M => A mapping for column headers
        P => A plaintext message
    Produces:
        C => A scrambled plaintext encoding
    Procedure:
        Create a straddling checkerboard => S
        Split P at a random point => (P1, P2)
        P2 + H (Message start) + P1 => P
        Encode P using S/M => C0
        Add nulls to C0 until size(C0) // 5 => C
*/

/**
Intermediate Keys:
    Given:
        S => Line from a song (min 20 characters)
        D => A date (formated as digits)
        N => A personal number (unique per agent)
    Produces:
        MI => A random 5 digit identifier
        K1 => Key for First Transposition
        K2 => Key for Second Transposition
        C => Straddling Checkerboard header
    Procedure:
        // TODO: Add back in the original procedure stuff
        Subtract (no borrowing) the first five date digits of D from MI => G0
        Expand G0 through chain addition to 10 digits => G1
        Add G1 to S1 (no carrying) => G
        Map S2 to 1234567890 and replace G accordingly => T
        Expand T through chain addition to 50 digits => T1
        Map T1 in 5 rows of 10 numbers => U
        Take the last two unique digits of U => (U1, U2)
        (U1 + N, U2 + N) => FT, ST
        Sequentialize T => F
        Read off U columnwise according to F => W
            column 0 then column 1 etc..
        Sequentialize the first FT digits of W => K1
        Sequentialize the next ST digits of W => K2
        Sequentialize the last row of U => C
*/

object VIC {
    private val r = scala.util.Random

    private def splitAt(s: String, i: Int) =
        List(s.substring(0, i).toList, s.substring(i).toList)

    private def halveString(s: String) = splitAt(s, s.length / 2)

    def interKeys(song: String, d: Int, n: Int) = {
        val s = halveString(song.replaceAll(" ", "").toUpperCase.substring(0, 20)).map(sequentialize(_))
        
        //val mi = r.nextInt(100000)             // Generate a 5 digit random number
        val mi = 60115
        val di = digits(d, 6)

        val g0 = (digits(mi), di.slice(0, 5)).zipped map((a, b) => ((a + 10) - b) % 10)
        val g = (chainAdd(g0.toList, 10), s(0)).zipped map((a, b) => (a + b) % 10)

        val t = g.map(a => s(1)((a + 9) % 10))

        val t1 = chainAdd(t, 60).slice(10, 60)
        val dis = t1.reverse.distinct.slice(0, 2).map(_ + n)        // (ST, FT)

        val u = columnTranspose(t1.map(a => if (a == 0) t1.size else a), 10).toList

        val f = sequentialize(t).zipWithIndex
                                .sortWith((a, b) => a._1 < b._1)
                                .flatMap(a => u(a._2))

        val k1 = sequentialize(f.slice(0, dis(1)))
        val k2 = sequentialize(f.slice(dis(1), dis.sum))
        val c = sequentialize(t1.reverse.take(10).reverse)

        (mi, k1, k2, c)
    }
}

class VIC extends Cipher {
    override def encrypt(msg: String) = msg
    override def decrypt(msg: String) = msg
}