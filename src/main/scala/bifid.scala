package crypto.classical;

import crypto.Cipher;
import crypto.utils.Polybius;

// TODO: Think of changing period to an option
class Bifid(period: Int) extends Cipher {
    private val sq = new Polybius(5, mixed(false))
    //sq.print

    def this() = this(0)

    // Note: This method removes spaces from the passed message
    def encrypt(msg: String) = {
        val groups = (for (c <- msg.replaceAll(" ",""))
            yield sq.translate(c))
                .grouped(if (period == 0) msg.length else period)
                .map(a => {
                    val (f, s) = a.unzip
                    (f ++ s).sliding(2, 2)
                })
        val res = for (frame <- groups; pair <- frame) yield sq.translate(pair(0), pair(1))
        
        res.mkString
    }

    def decrypt(msg: String) = {
        val p_length = if (period == 0) msg.length else period
        val groups = (for (c <- msg)
            yield sq.translate(c))
                .grouped(p_length)
                .map(a => {
                    val (f, s) = a.flatMap(t => Vector(t._1, t._2)).splitAt(p_length)              // Is there a method for this ???
                    f zip s
                })
        val res = for (frame <- groups; pair <- frame) yield sq.translate(pair)
        
        res.mkString
    }
}
