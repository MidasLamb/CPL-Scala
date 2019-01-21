package IMPLICIT_REFS

import BEGIN.BeginExp
import LET.{ConstExp, LetExp, Program, VarExp}
import org.scalatest.FlatSpec

class ImplicitRefsTest extends FlatSpec{
    "ImplicitRefs" should "run a simple program" in {
        val program = Program(
            LetExp('a, ConstExp(5), VarExp('a))
        )

        val result = ImplicitRefs.valueOf(program)
        assert(result == ImplicitRefs.NumVal(5))
    }

    it should "correctly reassign values" in {
        val program = Program(
            LetExp('a, ConstExp(5),
                BeginExp(List(
                    AssignExp('a, ConstExp(12)),
                    VarExp('a)
                ))
            )
        )

        val result = ImplicitRefs.valueOf(program)
        assert(result == ImplicitRefs.NumVal(12))

    }

}
