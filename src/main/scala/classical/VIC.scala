package crypto.classical;

import crypto.Cipher;
import crypto.utils.{sequentialize, chainAdd, digits, columnTranspose, Checkerboard};
// Procedure from http://everything2.com/user/raincomplex/writeups/VIC+cipher

// TODO: Improve encrypt implementation
// TODO: Improve organization
// TODO: Figure out how to decrypt

object VIC {
    private val r = scala.util.Random
    private val strt = "XYZ"

    private def splitAt(s: String, i: Int) =
        List(s.substring(0, i).toList, s.substring(i).toList)

    private def halveString(s: String) = splitAt(s, s.length / 2)

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
    def finalize(mi: Int, d: Int, p: List[Int]) = {
        val g = p.grouped(5).toList
        val (front, last) = g.splitAt(g.size - digits(d).last + 1)
        front ++ List(digits(mi).toList) ++ last
    }

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
    def secondTranspose(k2: List[Int], p: List[Int]) = {
        var iter = p.iterator
        val h = math.ceil(p.size / k2.size.toFloat).toInt
        val i = Stream.continually((1 to k2.size - 1).map(k2.indexOf(_))).flatten.iterator

        var t = Array.fill[Int](h, k2.size)(-1)
        var r = i.next

        for (n <- (0 to h - 2)) {
            val fill = iter.take(r)                         // Why do these iterators not need drop ???
            t(n) = t(n).map(a => if (fill.hasNext) fill.next else a)

            r = if (r == k2.size - 1) i.next else r + 1
        }

        val fill = iter.take(p.size % k2.size)
        t(h - 1) = t(h - 1).map(a => if (fill.hasNext) fill.next else a)

        val c = columnTranspose(
            t.flatMap(
                //_.mapWhile(() => iter.hasNext, a => if (a == -1) iter.next else a)
                _.map(a => {
                    if (a == -1)
                        if (iter.hasNext) iter.next
                        else 0      // Use '0' to handle 'null' for the moment
                    else a
                })).toList, k2.size)
            .map(_.reverse.dropWhile(_ == 0).reverse)       // Remove 0 from 
            .toList
        
        k2.zipWithIndex
          .sortWith(_._1 < _._1)
          .flatMap(a => c(a._2))
    }

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
    def firstTranspose(k1: List[Int], p: List[Int]) = {
        val num_over = p.length % k1.length
        val num_take = p.length / k1.length.toFloat
        val max = math.ceil(num_take).toInt
        val min = math.floor(num_take).toInt

        // Divide the message into columns
            // Note: Since the columns are shuffled in encryption, a simple grouping wouldn't be correct
        var iter = p.zipWithIndex
            .map(a => (a._1, a._2 % k1.length))
            .sortWith(_._2 < _._2)
            .map(_._1)

        // Collect the rows into columns
        val cols = (0 to k1.size - 1).map(a => {
            val num_items = if (a < num_over) max else min
            val ret = iter.take(num_items)
            iter = iter.drop(num_items)         // Update the iterator
            ret.toList
        }).toList

        k1.zipWithIndex
          .sortWith(_._1 < _._1)
          .flatMap(a => cols(a._2))
    }

    /**
    Straddling Checkerboard
        Given:
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
    def checker(msg: String, c: List[Int]) = {
        def adapt(p: (Int, Int)) = (if (p._1 == -1) -1 else c(p._1), c(p._2))

        //val board = new Checkerboard
        val board = new Checkerboard("ASINTOER  BDGJLPUWY.CFHKMQVXZ#")

        //val s = splitAt(msg.replaceAll(" ", ""), 14)//, r.nextInt(msg.size))
        //val p = s(1).mkString + strt + s(0).mkString
        val p = "IVESINVALIDATED.REPORTIMMEDIATELYTOSAFEHOUSE.AWAITEXTRACTIONINSTRUCTIONSWITHINWEEK..ASSIGNEDOBJECT"

        p.map(c => adapt(board.translate(c)))
         .flatMap(_ match {
            case (-1, x) => List(x)
            case (x, y) => List(x, y)
         }).toList
    }

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
    def interKeys(song: String, d: Int, n: Int) = {
        val s = halveString(song.replaceAll(" ", "").toUpperCase.substring(0, 20)).map(sequentialize(_))
        
        //val mi = r.nextInt(100000)             // Generate a 5 digit random number
        val mi = 60115
        val di = digits(d, 6)

        val g0 = (digits(mi), di.slice(0, 5)).zipped map((a, b) => ((a + 10) - b) % 10)
        val g = (chainAdd(g0.toList, 10), s(0)).zipped map((a, b) => (a + b) % 10)

        val t = g.map(a => s(1)((a + 9) % 10))                      // a - 1 in mod arithmetic

        val t1 = chainAdd(t, 60).slice(10, 60)
        val dis = t1.reverse.distinct.slice(0, 2).map(_ + n)        // Need to do distinct first to ensure that two distincts are found

        val u = columnTranspose(t1.map(a => if (a == 0) t1.size else a), 10).toList

        val f = sequentialize(t).zipWithIndex
                                .sortWith((a, b) => a._1 < b._1)
                                .flatMap(a => u(a._2))

        val k1 = sequentialize(f.slice(0, dis(1)))                   // The First Transpose is the second element of the tuple
        val k2 = sequentialize(f.slice(dis(1), dis.sum))
        val c = sequentialize(t1.reverse.take(10).reverse)

        (mi, k1, k2, c)
    }
}

class VIC(song: String, date: Int, n: Int) extends Cipher {
    override def encrypt(msg: String) = {
        val (mi, k1, k2, c) = VIC.interKeys("all the people are dead but I'm gonna keep dancing", 391752, 15)
        val c0 = VIC.checker("ASSIGNED OBJECTIVES INVALIDATED . REPORT IMMEDIATELY TO SAFE HOUSE . AWAIT EXTRACTION INSTRUCTIONS WITHIN WEEK", c)
        val c1 = VIC.firstTranspose(k1, c0)
        val c2 = VIC.secondTranspose(k2, c1)

        VIC.finalize(mi, 391752, c2).flatten.map(a => if (a == 10) 0 else a).mkString           // a must be <= 10
    }

    //msg map(_.asDigit)
    override def decrypt(msg: String) = msg
}