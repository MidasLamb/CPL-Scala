package IMPLICIT_REFS

import EXPLICIT_REFS.ExplicitRefs
import LET.{Expression, LetExp, VarExp}
import util.Util._

trait ImplicitRefs extends ExplicitRefs{
    override def valueOf(e: Expression, env: Environment): ExpVal = {
        e match {
            case AssignExp(identifier, expr) => {
                Store.setRef(env(identifier), valueOf(expr, env)) //We set the address the identifier is currently pointing at to something new.
                NumVal(27) //Total arbitrary number
            }
            case VarExp(v) => {
                val referenceToStore = env(v) // Get the reference to the store, which is stored in the Environment
                Store.deRef(referenceToStore) // Get the actual value
            }
            case LetExp(v, exp, body) => {
                val newRef = Store.newRef(valueOf(exp, env))
                val extendedEnv = ExtendEnv(v, RefVal(newRef), env)
                valueOf(body, extendedEnv)
            }
            case _ => super.valueOf(e, env)
        }
    }
}

object ImplicitRefs extends ImplicitRefs
