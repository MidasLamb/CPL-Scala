package MULTI_DECLARATION_LETREC

import LET.Let.BoolVal
import LET._
import LETREC.LetRec
import PROC._
import org.scalatest._
import MultiDeclarationLetRec.NumVal

class MultiDeclarationLetRecTest extends FlatSpec {
    "MultiDeclarationLetRec" should "correctly run a simple mutually recursive program." in {
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

        def evenOddGenerator(evenOrOdd: Symbol, x: Int):Program = {
            Program(
                LetRecExp(
                    List('even, 'odd),
                    List('x, 'x),
                    List(even, odd),
                    CallExp(VarExp(evenOrOdd), ConstExp(x))
                )
            )
        }

        val result0 = MultiDeclarationLetRec.valueOf(evenOddGenerator('even, 2))
        assert(result0 == NumVal(1))

        val result1 = MultiDeclarationLetRec.valueOf(evenOddGenerator('even, 0))
        assert(result1 == NumVal(1))

        val result2 = MultiDeclarationLetRec.valueOf(evenOddGenerator('odd, 1))
        assert(result2 == NumVal(1))

        val result3 = MultiDeclarationLetRec.valueOf(evenOddGenerator('even, 13))
        assert(result3 == NumVal(0))

        val result4 = MultiDeclarationLetRec.valueOf(evenOddGenerator('odd, 16))
        assert(result4 == NumVal(0))
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

        val result = Proc.valueOf(p1)
        assert(result == Proc.NumVal(66))
    }
}
