package LET
import LET.Let._

object Examples {
    def main(args: Array[String]) {
        val p = Program(ConstExp(3))
        assert(valueOf(p) == NumVal(3))
        println(p + " evaluates to:\n" + valueOf(p))
    }

    val p1 = Program(ConstExp(3))
    val p2 = Program(DiffExp(ConstExp(3), ConstExp(4)))
    val p3 = Program(
        DiffExp(
            ConstExp(10),
            DiffExp(ConstExp(3), ConstExp(4))))
    // x
    val p4 = Program(VarExp('x))
    // let x = 0 in x - 4
    val p5 = Program(LetExp('x, ConstExp(0), DiffExp(VarExp('x), ConstExp(4))))
    // let x = 0 in let x = 2 in x
    val p6 = Program(LetExp('x, ConstExp(0), LetExp('x, ConstExp(2), VarExp('x))))
    // xx
    val p7 = Program(VarExp('xx))
    val p8 = Program(LetExp('y, ConstExp(0), LetExp('y, DiffExp(VarExp('y), ConstExp(1)), VarExp('y))))
    val p9 = Program(ZeroExp(ConstExp(10)))
}
