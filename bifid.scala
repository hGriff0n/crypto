package crypto.classical;

import crypto.Cipher;
import crypto.utils.Polybius;

// TODO: Figure out what period means in terms of Bifid
class Bifid extends Cipher {
    private def mixed() = {
        val key = scala.util.Random.shuffle("ABCDEFGHIKLMNOPQRSTUVWXYZ".toList).mkString("")

        (iter: Int, x: Int, y: Int) => key(iter)
    }

    private val sq = new Polybius(5, mixed())

    def encrypt(msg: String) = {
        val (f, s) = (for (c <- msg) yield sq.translate(c)).unzip
        val res = for (pair <- (f ++ s).sliding(2, 2)) yield sq.translate(pair(0), pair(1))
        
        res.mkString("")
    }

    def decrypt(msg: String) = {
        val rows = (for (c <- msg) yield sq.translate(c)).toList.flatMap(t => List(t._1, t._2))
        val (f, s) = rows.splitAt(rows.size / 2)                        // Is there a method for this ???
        val res = for (pair <- f zip s) yield sq.translate(pair)
        
        res.mkString("")
    }
}
