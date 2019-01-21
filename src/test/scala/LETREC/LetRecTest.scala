package LETREC

import LET._
import PROC._
import org.scalatest._

class LetRecTest extends FlatSpec {
    "LetRec" should "correctly run a simple recursive program." in {
        //Program p.82
        val recursiveCall =
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
                    recursiveCall
                ),
                CallExp(VarExp('double), ConstExp(6))
            )
        )

        val result = LetRec.valueOf(p1)
        assert(result == LetRec.NumVal(12))
    }
}
