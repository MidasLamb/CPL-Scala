package EXPLICIT_REFS

import LET._
import PROC._

case class NewRefExp(expr: Expression) extends Expression {

}

case class DeRefExp(expr: Expression) extends Expression {

}

case class SetRefExp(expr1: Expression, expr2: Expression) extends Expression {

}

