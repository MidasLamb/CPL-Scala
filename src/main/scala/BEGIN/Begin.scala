package BEGIN

import LET.Expression
import MULTI_DECLARATION_LETREC.MultiDeclarationLetRec
import util.Util._

trait Begin extends MultiDeclarationLetRec {
    override def valueOf(e: Expression, env: Environment): ExpVal = {
        e match {
            case BeginExp(expressions) => {
                if (expressions.nonEmpty) {
                    for (i <- 0 until expressions.length - 1) {
                        valueOf(expressions(i), env)
                    }
                    valueOf(expressions.last, env)
                } else {
                    error("No expressions")
                }
            }
            case _ => super.valueOf(e, env)
        }
    }
}

object Begin extends Begin
