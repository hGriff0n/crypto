package crypto.classical

import crypto.utils.{shuffled, columnTranspose,CipherString}
import crypto.Context

class Trifid extends crypto.Cipher {
    private var con = new Context {
        val table = shuffled("ABCDEFGHIJKLMNOPQRSTUVWXYZ.").zipWithIndex.map(p => p._1 -> "%d%d%d".format(p._2 / 9, (p._2 % 9) / 3, p._2 % 9 % 3)).toMap
        val revLookup = table.map(_.swap)

        private def translate(msg: String, grouping: Int) = 
            columnTranspose(msg.ciphertext.map(table(_)).toList.flatten, grouping)
                .map(t => for (trip <- t.sliding(3, 3)) yield revLookup(trip.mkString))
                .map(_.mkString).mkString

        def >>(msg: String) = translate(msg, 3)
        def <<(msg: String) = translate(msg, msg.length).plaintext

        def view = table.mkString
    }

    override def encrypt(msg: String) = con >> msg
    override def decrypt(msg: String) = con << msg

    def context = con.view

    override def about = {
        // Generate intermediate representations
        val plaintext = "How are you"
        val ciphertext = encrypt(plaintext)

        val tablestr = (con.table map {
            case (k, v) => s"     $k  |  $v"
        }).grouped(3).map(_.mkString("\t    ")).mkString("\n\t    ")

        val ct = plaintext.ciphertext.map(con.table(_)).toList.flatten
        val ctr = columnTranspose(ct, 3).toList
        val codedtext = ct.mkString

        val otable = ct.grouped(3).map(_.mkString(" "))
        val ttable = ctr.map(_.mkString(" "))
        val transposedtablestr = otable.zipWithIndex.map(s => "      " + (s._2 match {
            case i if (i == 3 || i == 5) => s._1 + "             " + ttable(s._2 - 3)
            case i if (i == 4) => s._1 + "      =>     " + ttable(2)
            case i => s._1
        })).mkString("\n\t    ")

        val finaltext = ctr.map(_.mkString).mkString

        val len = ciphertext.length

    s"""
        A trifid cipher is an extension of the polybius square into the third dimension.
        Instead of creating a 5x5 or 6x6 square to handle the translation, a trifid cipher
        Is formed by taking a 3x3x3 cube which each "block" inhabited by a single letter.
        {TODO: Add history?}
        
        Just like with a polybius square, coding with a trifid cube involves translating
        A character to and from its coordinates in the cubic fields. However, with the
        Addition of the third dimension it is often simpler to represent this cubic field
        As a simple table, directly mapping the character to its replacement coordinates:

              ASCII | Coord           ASCII | Coord           ASCII | Coord
            -----------------       -----------------       -----------------
            $tablestr

        Enciphering in a trifid cipher is slightly more involved than the simple translation
        with the cubic field. For this demo, we will be enciphering the example plaintext
        String, \"$plaintext\". The first step is to run the message through the cubic field,
        transforming each character into its coordinate triple.

            $plaintext => $codedtext

        The encoded coordinate string is then written out row-wise in a 3-column table.
        The created table is then read column-wise to produce the final coded string.

              Written Table  |         Read Table
            --------------------------------------------
            $transposedtablestr

        Finally this new string, split into coordinate triplets is then feed back through the
        Cubic field to get the corresponding character. Since the field is 3x3x3 and allowed
        characters in the string are in [0..2], this process is guaranteed to produce.

            $finaltext => $ciphertext

        Decryption of a trifid cipher is almost the exact same process as encryption.
        The only difference between the two infact lies in the width of the table used
        To perform the columnar transposition. In the encryption stage, this table is 3
        Characters wide, in order to promote effective dispersion. In the decryption stage,
        The constructed table is instead allowed to have $len columns, effectively restricting
        It's height to 3, thereby enabling the transposition process to reset the ordering. 
    """
    }
}