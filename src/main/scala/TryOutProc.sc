import LET._
import PROC._
import PROC.Proc._

/**
  * BE SURE TO BUILD THE PROJECT BEFORE RUNNING THESE WORKSHEETS. IT DOES NOT BUILD THE PROJECT FOR YOU!
  */
object TryOutProc {
    println("Welcome to trying out proc.")

    val p1 = Program(
        LetExp(
            'f,
            ProcExp('x,
                DiffExp(VarExp('x), ConstExp(11))
            ),
            CallExp(VarExp('f), ConstExp(77))
        )
    )

    valueOf(p1)
}