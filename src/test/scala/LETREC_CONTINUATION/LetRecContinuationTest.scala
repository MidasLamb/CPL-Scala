package LETREC_CONTINUATION

import org.scalatest._

class LetRecContinuationTest extends FlatSpec {
    "LetRecContinuation" should "correctly run a simple recursive program." in {
        //Program p.82
        val functionDoubleBody =
            DiffExp(
                CallExp(VarExp('double),
                    DiffExp(VarExp('x), ConstExp(1))
                ),
              ConstExp(-2)
            )
        val p1 = Program(
            LetRecExp(
                'double,
                'x,
                IfExp(ZeroExp(VarExp('x)),
                    ConstExp(0),
                    functionDoubleBody
                ),
                CallExp(VarExp('double), ConstExp(6))
            )
        )

        val result = LetRecContinuation.valueOf(p1)
        assert(result == LetRecContinuation.NumVal(12))
    }

    it should "properly run proc functions, since it's an extension" in {
        val p1 = Program(
            LetExp(
                'f,
                ProcExp('x,
                    DiffExp(VarExp('x), ConstExp(11))
                ),
                CallExp(VarExp('f), ConstExp(77))
            )
        )

        val result = LetRecContinuation.valueOf(p1)
        assert(result == LetRecContinuation.NumVal(66))
    }

    it should "properly run the counter function" in {
        val program = Program(
            LetRecExp(
                'counter,
                'x,
                IfExp(ZeroExp(VarExp('x)),
                    ConstExp(0),
                    DiffExp(CallExp(VarExp('counter), DiffExp(VarExp('x), ConstExp(1))), ConstExp(-1))
                ),
                CallExp(VarExp('counter), ConstExp(4))
            )
        )

        val result = LetRecContinuation.valueOf(program)
        assert(result == LetRecContinuation.NumVal(4))
    }
}
