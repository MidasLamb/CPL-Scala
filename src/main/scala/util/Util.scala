package util

object Util {

    class EvalException(s: String) extends Exception

    def error(s: String): Nothing = throw new EvalException(s)
}
