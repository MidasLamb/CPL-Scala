package LETREC

import LET._
import PROC._

case class LetRecExp(procName: Symbol, boundVar: Symbol, procBody: Expression, letrecBody: Expression) extends Expression {

}
