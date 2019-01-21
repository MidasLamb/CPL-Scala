package PROC
import LET._
import util.Util._

/**
  * Creates a ProcVal with a closure, meaning the environment on creation is captured. Some languages, such as JS don't always have this.
  */
trait Proc extends Let{

    case class ProcVal(variable: Symbol, body: Expression, env: Environment) extends ExpVal {
        override def toNum: Int = ???

        override def toBool: Boolean = ???

        override def toString: String = s"ProcVal($variable) {$body} [$env]"
    }

    override def valueOf(e: Expression, env: Environment): ExpVal = {
        e match {
            case ProcExp(variable, body) => ProcVal(variable, body, env)
            case CallExp(rator, rand) => {
                val ratorVal = valueOf(rator, env)
                val randVal = valueOf(rand, env)
                applyProcedure(ratorVal, randVal)
            }
            case _ => super.valueOf(e, env)
        }
    }

    def applyProcedure(proc: ExpVal, value: ExpVal): ExpVal = {
        proc match {
            case ProcVal(variable, body, env) => {
                val extendedEnv = ExtendEnv(variable, value, env)
                valueOf(body, extendedEnv)
            }
            case _ => error(s"Cannot apply $proc since it is not a ProcVal.")
        }
    }
}

object Proc extends Proc