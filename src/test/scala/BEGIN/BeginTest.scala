package BEGIN
import LET._
import MULTI_DECLARATION_LETREC.LetRecExp
import PROC.CallExp
import org.scalatest._

class BeginTest extends FlatSpec {
    "Begin" should "properly return the value of the last expression" in {
        val even =
            IfExp(ZeroExp(VarExp('x)),
                ConstExp(1),
                CallExp(VarExp('odd), DiffExp(VarExp('x), ConstExp(1)))
            )
        val odd =
            IfExp(ZeroExp(VarExp('x)),
                ConstExp(0),
                CallExp(VarExp('even), DiffExp(VarExp('x), ConstExp(1)))
            )

        val program = Program(
                LetRecExp(
                    List('even, 'odd),
                    List('x, 'x),
                    List(even, odd),
                    BeginExp(
                        List(
                            CallExp(VarExp('even), ConstExp(2)),
                            CallExp(VarExp('odd), ConstExp(2))
                        )
                    )
                )
            )

        val result = Begin.valueOf(program)
        assert(result == Begin.NumVal(0))

        val program2 = Program(
            LetRecExp(
                List('even, 'odd),
                List('x, 'x),
                List(even, odd),
                BeginExp(
                    List(
                        CallExp(VarExp('odd), ConstExp(2)),
                        CallExp(VarExp('even), ConstExp(2))
                    )
                )
            )
        )

        val result2 = Begin.valueOf(program2)
        assert(result2 == Begin.NumVal(1))
    }
}
