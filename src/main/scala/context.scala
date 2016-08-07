package crypto

trait Context {
    def >>(msg: String): String
    def <<(msg: String): String

    def view: String
}