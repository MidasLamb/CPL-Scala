package EXPLICIT_REFS

import BEGIN.BeginExp
import LET.{ConstExp, LetExp, Program, VarExp}
import org.scalatest.FlatSpec

class ExplicitRefsTest extends FlatSpec{
    "ExplicitRefs" should "properly work with simple references." in {
        val p = Program(
            LetExp(
                'x,
                NewRefExp(NewRefExp(ConstExp(0))),
                BeginExp(
                    List(
                        SetRefExp(DeRefExp(VarExp('x)), ConstExp(11)),
                        DeRefExp(DeRefExp(VarExp('x)))
                    )
                )
            )
        )

        val result = ExplicitRefs.valueOf(p)
        assert(result == ExplicitRefs.NumVal(11))
    }

}
