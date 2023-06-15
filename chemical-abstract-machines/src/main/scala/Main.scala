package example 

import io.chymyst.jc._
import org.slf4j.LoggerFactory
import scala.util.Random

trait Gate 
case object X extends Gate 
case class H(ctrl: Int) extends Gate 
case class CX(ctrl: Int, target: Int) extends Gate
case object NullGate extends Gate
case object Measure extends Gate

final case class Circuit(remainingGates: List[Gate]) 
final case class QVec(prop: Double, v: Vector[Boolean])

object Main extends App {

    val hScale : Double = 1.0 / Math.sqrt(2.0)
    val r = m[QVec]
    val f = m[QVec]
    val d = m[Unit]
    val logger = LoggerFactory.getLogger("example.Main")
    val decider = new Random()

    site (
        go { case r(a) + r(b) ⇒ 
                logger.debug(s"Possible Reaction between $a and $b"); 
                if(a.v.sameElements(b.v)){
                    logger.debug("Molecules compatible. Reaction starting")
                    val newQ = QVec(a.prop + b.prop, a.v)
                    if(newQ.prop.abs > .4){
                        logger.info("I am tired. Time to go: " + newQ)
                        f(newQ)
                    }else{
                        r(newQ)
                    }
                }else{
                    logger.debug("Incompatible molecules. Returning molecules to the pool")
                    r(a) + r(b)
                }
        },

        go {case f(a) + f(b) ⇒ 
                if(decider.nextInt(1) > 0){
                    logger.info("Eliminated " + b + " , Remains: " + a)
                    f(a)
                }else{
                    logger.info("Eliminated " + a + ", Remains: " + b)
                    f(b)
                }

        }
    )

    def build(c: Circuit, v: QVec) : Unit = {
        c.remainingGates match{ 
            case Nil => 
                logger.debug("Releasing " + v)
                r(v)
            case x :: xs => 
                x match {
                    case H(t) => 
                        val sign = if (v.v(t)) -1 else 1
                        val x1 = QVec(sign * hScale * v.prop, v.v)
                        val x2 =  QVec(hScale * v.prop, v.v.updated(t, !v.v(t)))
                        build(Circuit(xs), x1)
                        build(Circuit(xs), x2)
                    case CX(c, t) => 
                        if(v.v(c)){
                            build(Circuit(xs), QVec(v.prop, v.v.updated(t, !(v.v(t)))))
                        }else{
                            build(Circuit(xs), QVec(v.prop, v.v))
                        }
                }
        }
    }

    build(Circuit(List(H(0), H(1), CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), H(0), H(1))),
         QVec(1d, Vector(false, false, false, false)))
}