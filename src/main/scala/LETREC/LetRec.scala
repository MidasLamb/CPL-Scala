package LETREC

import LET.Expression
import LETREC._
import PROC._

/**
  * Allow for recursive calls. Proc does not allow this because with let the environment passed to the ProcVal does not yet contain itself.
  *
  * Another way to look at this is to remind yourself of closures. If you give a ProcVal the environment on creation, to create a closure, the ProcVal itself
  * is not yet part of this environment, so you cannot call it from within itself.
  *
  * We allow recursion by delaying the creation of the ProcVal until we search through the environment, allowing
  * the ProcVal to have an environment with itself in it.
  */
class LetRec extends Proc {
    override def valueOf(e: Expression, env: Environment): ExpVal = {
        e match {
            case LetRecExp(procName, boundVar, procBody, letrecBody) => {
                // ExtendEnvRec creates an environment with a delayed ProcVal
                val extendedEnvRec = ExtendEnvRec(procName, boundVar, procBody, env)
                valueOf(letrecBody, extendedEnvRec)
            }
            case _ => super.valueOf(e, env)
        }
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

}

object LetRec extends LetRec