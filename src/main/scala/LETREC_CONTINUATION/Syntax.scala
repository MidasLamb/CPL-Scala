package LETREC_CONTINUATION

case class Program(exp: Expression) {
    override def toString() = exp.toString
}

class Expression

case class ConstExp(num: Int) extends Expression {
    override def toString() = num.toString
}

case class BoolExp(bool: Boolean) extends Expression {
    override def toString: String = bool.toString
}

case class DiffExp(exp1: Expression, exp2: Expression) extends Expression {
    override def toString() = "( " + exp1 + " - " + exp2 + " )"
}

case class ZeroExp(exp: Expression) extends Expression {
    override def toString() = "zero? " + exp
}


case class IfExp(exp1: Expression, exp2: Expression, exp3: Expression) extends Expression {
    override def toString() = "if " + exp1 + " then " + exp2 + " else " + exp3
}

case class VarExp(v: Symbol) extends Expression {
    override def toString() = v.name
}

case class LetExp(v: Symbol, exp: Expression, body: Expression) extends Expression {
    override def toString() = "let " + v.name + " = " + exp + " in " + body
}

case class ProcExp(identifier: Symbol, body: Expression) extends Expression {
    override def toString() = s"proc ($identifier) { $body }"
}

case class CallExp(rator: Expression, rand: Expression) extends Expression {
    override def toString() = s"$rator($rand)"
}

case class LetRecExp(procName: Symbol, boundVar: Symbol, procBody: Expression, letrecBody: Expression) extends Expression {

}

