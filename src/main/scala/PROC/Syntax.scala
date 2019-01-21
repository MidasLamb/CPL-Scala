package PROC
import LET._

case class ProcExp(identifier: Symbol, body: Expression) extends Expression {
    override def toString() = s"proc ($identifier) { $body }"
}

case class CallExp(rator: Expression, rand: Expression) extends Expression {
    override def toString() = s"$rator($rand)"
}

