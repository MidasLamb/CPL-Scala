package MULTI_DECLARATION_LETREC

import LET.{Expression, Let}
import PROC.Proc

/**
  * It is important to note that this is an extension on Proc and not on LetRec. There is a fundamentally difference in how the LetRec functions get represented,
  * even when there is only one function.
  */
trait MultiDeclarationLetRec extends Proc with Let{
    override def valueOf(e: Expression, env: Environment): ExpVal = {
        e match {
            case LetRecExp(procNames, boundVars, procBodies, letrecBody) => {
                // ExtendEnvRec creates an environment with a delayed ProcVal
                val extendedEnvRec = ExtendEnvRec(procNames, boundVars, procBodies, env)
                valueOf(letrecBody, extendedEnvRec)
            }
            case _ => super.valueOf(e, env)
        }
    }

    /**
      * We must extend the environment all at once, because otherwise the first function wouldn't be able
      * to call the last function, if we just chained environment creation (env = ExtendEnvRec(p(0),b(0),pb(0), ExtendEnvRec(p(1),...
      */
    case class ExtendEnvRec(procNames: List[Symbol], boundVars: List[Symbol], procBodies: List[Expression], environment: Environment) extends Environment {
        def apply(s: Symbol) = {
            if (procNames.contains(s)) {
                val index = procNames.indexOf(s)
                ProcVal(boundVars(index), procBodies(index), this)
            } else {
                environment(s)
            }
        }
    }
}

object MultiDeclarationLetRec extends MultiDeclarationLetRec

