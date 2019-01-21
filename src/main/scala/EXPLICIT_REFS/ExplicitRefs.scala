package EXPLICIT_REFS

import BEGIN.Begin
import LET.{Expression, Program}
import util.Util._

import scala.collection.mutable.ListBuffer

trait ExplicitRefs extends Begin{
    type Ref = Int

    object Store {
        val storage: ListBuffer[ExpVal] = ListBuffer[ExpVal]()

        def newRef(exp: ExpVal): Ref = {
            storage.append(exp)
            storage.length - 1
        }

        def deRef(ref: Ref): ExpVal = {
            storage(ref)
        }

        def setRef(ref: Ref, expr: ExpVal) = {
            storage(ref) = expr
        }
    }

    case class RefVal(ref: Ref) extends ExpVal {
        override def toNum: Int = ???

        override def toBool: Boolean = ???
    }

    override def valueOf(p: Program): ExpVal = {
        // We should initialize the store but that is unnecessary because it already starts empty.
        p match {
            case Program(e) => valueOf(e, initEnv)
        }
    }

    override def valueOf(e: Expression, env: Environment): ExpVal = {
        e match {
            case NewRefExp(expr) => {
                val ref = Store.newRef(valueOf(expr, env))
                RefVal(ref)
            }
            case DeRefExp(expr) => {
                val ref = valueOf(expr, env)
                ref match {
                    case RefVal(ref) => {
                        Store.deRef(ref)
                    }
                    case _ => error("Not a ref value")
                }
            }
            case SetRefExp(expr1, expr2) => {
                val ref = valueOf(expr1, env)
                ref match {
                    case RefVal(ref) => {
                        Store.setRef(ref, valueOf(expr2, env))
                        NumVal(23) // Completely arbitrary number.
                    }
                    case _ => error("Not a ref value")
                }
            }
            case _ => super.valueOf(e, env)
        }
    }
}

object ExplicitRefs extends ExplicitRefs
