package MULTI_DECLARATION_LETREC

import LET._
import PROC._


case class LetRecExp(procName: List[Symbol], boundVar: List[Symbol], procBody: List[Expression], letrecBody: Expression) extends Expression {

}
