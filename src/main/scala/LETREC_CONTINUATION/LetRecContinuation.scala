package LETREC_CONTINUATION

import util.Util._

/**
  * Continuations are the control flow of the program.
  *
  * The execution is now a game of pingpong between Continuation(expVal) and valueOf(Expression); Continuations might need to know the
  * valueOf certain expressions to return, this in turn can create more Continuations to 'tack on' the existing ones.
  *
  * If you think about it, you start with one massive expression
  */
trait LetRecContinuation {
    abstract class ExpVal {
        def toNum: Int

        def toBool: Boolean
    }

    case class NumVal(v: Int) extends ExpVal {
        def toNum = v

        def toBool = error("Can't convert number to boolean")

        override def toString = v.toString
    }

    case class BoolVal(b: Boolean) extends ExpVal {
        def toNum = error("Can't convert boolean to number")

        def toBool = b

        override def toString = b.toString
    }

    case class ProcVal(variable: Symbol, body: Expression, env: Environment) extends ExpVal {
        override def toNum: Int = ???

        override def toBool: Boolean = ???

        override def toString: String = s"ProcVal($variable) {$body} [$env]"
    }

    abstract class Environment {
        def apply(s: Symbol): ExpVal
    }

    case object EmptyEnv extends Environment {
        def apply(s: Symbol) = error("unbound var")

        override def toString: String = ""
    }

    case class ExtendEnv(bvar: Symbol, bval: ExpVal, saved: Environment) extends Environment {
        def apply(s: Symbol) = if (s == bvar) bval else saved(s)

        override def toString: String = s"$bvar: $bval || $saved"
    }

    case class ExtendEnvRec(procName: Symbol, boundVar: Symbol, procBody: Expression, environment: Environment) extends Environment {
        def apply(s: Symbol) = {
            if (s == procName) {
                // We create the procval here, giving this environment self as the environment.
                ProcVal(boundVar, procBody, this)
            } else {
                environment(s) // We search through the previous environments till we find the correct one or none.
            }
        }

        override def toString: String = s"$procName($boundVar): $procBody || $environment"
    }

    val initEnv = ExtendEnv('i, NumVal(1), ExtendEnv('v, NumVal(5), ExtendEnv('x, NumVal(10), EmptyEnv)))

    type FinalAnswer = ExpVal

    abstract class Continuation {
        def apply(expVal: ExpVal): FinalAnswer
    }

    case class EndCont() extends Continuation {
        def apply(expVal: ExpVal): FinalAnswer = {
            println(s"End of computation: $expVal")
            expVal
        }
    }

    case class ZeroCont(cont: Continuation) extends  Continuation {
        def apply(expVal: ExpVal): FinalAnswer = {
            cont(BoolVal(expVal.toNum == 0))
        }
    }

    case class LetExpCont(v: Symbol, body: Expression, env: Environment, cont: Continuation) extends Continuation {
        def apply(expVal: ExpVal): FinalAnswer = {
            val extendedEnv = ExtendEnv(v, expVal, env)
            valueOf(body, extendedEnv, cont)
        }
    }

    case class IfTestCont(exp1: Expression, exp2: Expression, env: Environment, cont: Continuation) extends Continuation {
        def apply(expVal: ExpVal): FinalAnswer = {
            expVal match {
                case BoolVal(b) => {
                    if (b) {
                        valueOf(exp1, env, cont)
                    } else {
                        valueOf(exp2, env, cont)
                    }
                }
                case _ => error("Not a boolean value")
            }
        }
    }

    case class Diff1Cont(exp2: Expression, env: Environment, cont: Continuation) extends Continuation {
        def apply(expVal: ExpVal): FinalAnswer = {
            valueOf(exp2, env, Diff2Cont(expVal, cont))
        }
    }

    case class Diff2Cont(expVal1: ExpVal, cont: Continuation) extends Continuation {
        def apply(expVal2: ExpVal): FinalAnswer = {
            (expVal1, expVal2) match  {
                case (NumVal(num1), NumVal(num2)) => {
                    cont(NumVal(num1 - num2))
                }
                case _ => error("Not a NumVal for one of the two.")
            }
        }
    }

    case class RatorCont(rand: Expression, env: Environment, cont: Continuation) extends Continuation {
        def apply(expVal: ExpVal): FinalAnswer = {
            valueOf(rand, env, RandCont(expVal, cont))
        }
    }

    case class RandCont(ratorVal: ExpVal, cont: Continuation) extends Continuation {
        def apply(expVal: ExpVal): FinalAnswer = {
            ratorVal match {
                case procVal: ProcVal => {
                    applyProcedure(procVal, expVal, cont)
                }
            }
        }
    }


    val initCont = EndCont()

    def valueOf(p: Program): FinalAnswer = {
        p match {
            case Program(e) => valueOf(e, initEnv, initCont)
        }
    }

    def valueOf(e: Expression, env: Environment, cont: Continuation): ExpVal = {
        e match {
            case ConstExp(num) => cont(NumVal(num))
            case VarExp(v) => cont(env(v))
            case ProcExp(variable, body) => cont(ProcVal(variable, body, env))
            case DiffExp(exp1, exp2) => {
                valueOf(exp1, env, Diff1Cont(exp2, env, cont))
            }
            case ZeroExp(exp) => valueOf(exp, env, ZeroCont(cont))
            case IfExp(exp1, exp2, exp3) => {
                valueOf(exp1, env, IfTestCont(exp2, exp3, env, cont))
            }
            case LetExp(v, exp, body) => {
                val newContinuation = LetExpCont(v, body, env, cont)
                valueOf(exp, env, newContinuation)
            }
            case CallExp(rator, rand) => {
                valueOf(rator, env, RatorCont(rand, env, cont))
            }
            case LetRecExp(procName, boundVar, procBody, letrecBody) => {
                // ExtendEnvRec creates an environment with a delayed ProcVal
                val extendedEnvRec = ExtendEnvRec(procName, boundVar, procBody, env)
                valueOf(letrecBody, extendedEnvRec, cont)
            }
        }
    }

    def applyProcedure(proc: ExpVal, value: ExpVal, cont: Continuation): FinalAnswer = {
        proc match {
            case ProcVal(variable, body, env) => {
                val extendedEnv = ExtendEnv(variable, value, env)
                valueOf(body, extendedEnv, cont)
            }
            case _ => error(s"Cannot apply $proc since it is not a ProcVal.")
        }
    }
}

object LetRecContinuation extends LetRecContinuation