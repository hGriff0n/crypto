package crypto;

trait Cipher {
    protected val lc = 'a' to 'z'
    protected val uc = 'A' to 'Z'
    
    protected def mixed(incl_num: Boolean) = {
        val key = scala.util.Random.shuffle(
            (if (incl_num) "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" else "ABCDEFGHIKLMNOPQRSTUVWXYZ")
            .toList).iterator

        (iter: Int, x: Int, y: Int) => key.next
    }

    def encrypt(msg: String): String
    def decrypt(msg: String): String
}

class Iden extends Cipher {
    def encrypt(msg: String) = msg
    def decrypt(msg: String) = msg
}
