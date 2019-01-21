package PROC
import LET._
import org.scalatest._
import util.Util.EvalException

class ProcTest extends FlatSpec {
    "Proc" should "correctly run a simple program." in {
        val p1 = Program(
            LetExp(
                'f,
                ProcExp('x,
                    DiffExp(VarExp('x), ConstExp(11))
                ),
                CallExp(VarExp('f), ConstExp(77))
            )
        )

        val result = Proc.valueOf(p1)
        assert(result == Proc.NumVal(66))
    }

    it should "not work for recursive calls." in {
        val p1 = Program(
            LetExp(
                'f,
                ProcExp('x,
                    DiffExp(VarExp('x), CallExp(VarExp('f), ConstExp(1)))
                ),
                CallExp(VarExp('f), ConstExp(77))
            )
        )

        assertThrows[EvalException] {
            Proc.valueOf(p1)
        }
    }
}
