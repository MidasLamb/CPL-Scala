package LET
import util.Util._

trait Let {

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

    val initEnv = ExtendEnv('i, NumVal(1), ExtendEnv('v, NumVal(5), ExtendEnv('x, NumVal(10), EmptyEnv)))

    def valueOf(p: Program): ExpVal = {
        p match {
            case Program(e) => valueOf(e, initEnv)
        }
    }

    def valueOf(e: Expression, env: Environment): ExpVal = {
        e match {
            case ConstExp(num) => NumVal(num)
            case DiffExp(exp1, exp2) => NumVal(valueOf(exp1, env).toNum - valueOf(exp2, env).toNum)
            case ZeroExp(exp) => BoolVal(valueOf(exp, env).toNum == 0)
            case IfExp(exp1, exp2, exp3) => if (valueOf(exp1, env).toBool) valueOf(exp2, env) else valueOf(exp3, env)
            case VarExp(v) => env(v)
            case LetExp(v, exp, body) => {
                val extendedEnv = ExtendEnv(v, valueOf(exp, env), env)
                valueOf(body, extendedEnv)
            }
        }
    }
}

object Let extends Let
