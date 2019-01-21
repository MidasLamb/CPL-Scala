package IMPLICIT_REFS

import LET._
import PROC._

/**
  * We need to be able to change the contents of a location (p.116)
  * @param symbol
  * @param expr
  */
case class AssignExp(symbol: Symbol, expr: Expression) extends Expression {

}

