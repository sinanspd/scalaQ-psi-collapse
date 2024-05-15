package example2 

import io.chymyst.jc._
import org.slf4j.LoggerFactory
import scala.util.Random
import scala.math._
import spire.math._
import spire.implicits._ 
import scala.util.control.NoStackTrace
import fs2._
import cats.effect.IO
import cats.Eval
import cats.effect.unsafe.implicits.global
import com.sinanspd.qure.circuit._
import com.sinanspd.qure.circuit.gates._
import com.sinanspd.qure.circuit.circuitError._
import com.sinanspd.qure.circuit.sampler._
import com.sinanspd.qure.circuit.sampler.BasicSampler
import cats.instances.int

//Fully Cham implementation 
object Cham2 extends App {

    val correctAnswers = List() 
    val concurrentInstances = 5 // Fill this out before running 
    
    val logger = LoggerFactory.getLogger("example2.Cham2")
    val hScale : Double = 1.0 / Math.sqrt(2.0)
    val decider = new Random()

    val i = m[QVec]
    val e = m[(QVec, Circuit)]
    val step = m[Unit]
    var med = false

    implicit class CircuitDeferral(c: Circuit){ 
        def deferMeasurements = {
            val rearrangedGates = c.remainingGates.foldLeft(List[Gate]())((a, b) => b match{
                case b : DeferredMeasurement if(a.isEmpty) => 
                    throw InvalidCircuitError("A DeferredMeasurement gate shouldn't appear at the beginning of a circuit")
                case b : DeferredMeasurement => {
                    val prevGate = a.takeRight(1).head
                    prevGate match{
                        case g: DeferredMeasurement => a.take(a.size - 1) :+ MultiDeferredMeasure(List(g.q, b.q))
                        case _ => a :+ b
                    }
                }
                case b : Measure => a :+ b
                case _ => 
                    if(a.isEmpty){
                        a :+ b 
                    }else{
                        val prevGate = a.takeRight(1).head
                         prevGate match{
                            case DeferredMeasurement(_) | MultiDeferredMeasure(_) => 
                                a.take(a.size - 1) ++ List(b, prevGate)
                            case _ => a :+ b
                        }
                    }
            })

            new Circuit(rearrangedGates)
        }
    }

    object Circuit{
        def apply(remainingGates: List[Gate]) = new Circuit(remainingGates).deferMeasurements
    }

    case class MultiDeferredMeasure(q: List[Int]) extends Gate 
    case class DeferredMeasurement(q: Int) extends Gate 

    site (
        go { case i(a) + i(b) ⇒ { //two intermediate results ready to interfere 
                logger.debug(s"Possible Reaction between $a and $b"); 
                if(a.v.sameElements(b.v)){ 
                    logger.debug("Molecules compatible. Reaction starting")
                    val newQ = QVec(a.prop + b.prop, a.v)
                    if(newQ.prop.real.abs >= 0.9){
                        if(!med){
                            logger.debug(s"Threshold Passed")
                            med = true
                            scaleAndSample()
                        }
                    }else if(newQ.prop.real.abs == 0d){
                        logger.debug("Destrictive Interference")
                    }else{
                        logger.debug(s"Reaction finished, releasesing $newQ into the solution")
                        i(newQ)
                    }
                }else{
                    logger.debug("Incompatible molecules. Returning molecules to the pool")
                    i(a) + i(b)
                }
            }
        },
        go {
            case e(a) + step(()) ⇒ {
                if(a._2.remainingGates.length == 1){ //whatever we release here will be ready for interference
                    val v = a._1
                    applyGate(a._2.remainingGates, v, true)
                }else{
                    val v = a._1
                    applyGate(a._2.remainingGates, v, false)
                }
            }
        }
    )

    def applyGate(gates: List[Gate], v: QVec, fin: Boolean) =  // we need to re-release a step() every time otherwise there wont be enough in the pool 
        gates.head match {
            case X(t) => {
                val nv = QVec(v.prop, v.v.updated(t, !v.v(t)))
                if(fin){
                    i(nv) + step(())
                }else{
                    e((nv, Circuit(gates.tail))) + step(())
                }
            }
            case H(t) => {
                val sign = if (v.v(t)) {-1} else {1}
                if(fin){    
                    i(QVec(sign * hScale * v.prop, v.v)) + i(QVec(hScale * v.prop, v.v.updated(t, !v.v(t)))) + step(())
                }else{
                    e((QVec(sign * hScale * v.prop, v.v), Circuit(gates.tail))) + e((QVec(hScale * v.prop, v.v.updated(t, !v.v(t))), Circuit(gates.tail))) + step(()) + step(())
                }
            }
            case CX(c, t) => 
                if(fin){
                    if(v.v(c)){
                        i(QVec(v.prop, v.v.updated(t, !(v.v(t))))) + step(())
                    }else{
                        i(QVec(v.prop, v.v)) + step(())
                    }
                }else{
                    if(v.v(c)){
                        e((QVec(v.prop, v.v.updated(t, !(v.v(t)))), Circuit(gates.tail))) + step(())
                    }else{
                        e((QVec(v.prop, v.v) , Circuit(gates.tail))) + step(())
                    }
                }
            case Swap(q1, q2) => 
                val source = v.v(q1)
                val target = v.v(q2)
                val firstOverwrite = v.v.updated(q2, source)
                val second = firstOverwrite.updated(q1, target)
                if(fin){
                    i(QVec(v.prop, second)) + step(())
                }else{
                    e((QVec(v.prop, second), Circuit(gates.tail))) + step(())    
                }
            case RZ(td, q) => 
                val target = v.v(q)
                val coeff = td match{
                    case -2 => 1/0.5
                    case 2 => 0.5
                    case -4 => 1/0.25
                    case 4 => 0.25
                    case -8 => 1/0.125
                    case 8 => 0.125
                    case -16 => 1/0.0625
                    case 16 => 0.0625
                }
                if(fin){
                    if(target){
                        i(QVec(v.prop * new Complex(0d, 1d) * coeff , v.v)) + step(())
                    }else{
                        i(QVec(v.prop * -1 * new Complex(0d, 1d) * coeff, v.v)) + step(())
                    }
                }else{
                    if(target){
                        e(QVec(v.prop * new Complex(0d, 1d) * coeff , v.v) , Circuit(gates.tail)) + step(())
                    }else{
                        e(QVec(v.prop * -1 * new Complex(0d, 1d) * coeff, v.v), Circuit(gates.tail)) + step(())
                    }
                }
            case Rotate(td, q) => 
                if(fin){
                    td match{
                        case -2 => i(QVec(v.prop * 1/0.5, v.v)) + step(())
                        case 2 => i(QVec(v.prop * 0.5, v.v)) + step(())
                        case -4 => i(QVec(v.prop * 1/0.25, v.v)) + step(())
                        case 4 => i(QVec(v.prop * 0.25, v.v)) + step(())
                        case -8 => i(QVec(v.prop * 1/0.125, v.v)) + step(())
                        case 8 => i(QVec(v.prop * 0.125, v.v)) + step(())
                        case -16 => i(QVec(v.prop * 1/0.0625, v.v)) + step(())
                        case 16 => i(QVec(v.prop * 0.0625, v.v)) + step(())
                    }
                }else{
                    td match{
                        case -2 => e(QVec(v.prop * 1/0.5, v.v), Circuit(gates.tail)) + step(())
                        case 2 => e(QVec(v.prop * 0.5, v.v), Circuit(gates.tail)) + step(()) 
                        case -4 => e(QVec(v.prop * 1/0.25, v.v), Circuit(gates.tail)) + step(())
                        case 4 => e(QVec(v.prop * 0.25, v.v), Circuit(gates.tail)) + step(()) 
                        case -8 => e(QVec(v.prop * 1/0.125, v.v), Circuit(gates.tail)) + step(())
                        case 8 => e(QVec(v.prop * 0.125, v.v), Circuit(gates.tail)) + step(())
                        case -16 => e(QVec(v.prop * 1/0.0625, v.v), Circuit(gates.tail)) + step(())
                        case 16 => e(QVec(v.prop * 0.0625, v.v), Circuit(gates.tail)) + step(())
                    }
                }
            case CRotate(c, td, q) => 
                val target = v.v(c) 
                if(target == true && v.v(q) == true){
                    if(fin){
                        td match{
                            case -2 => i(QVec(v.prop * -0.5, v.v)) + step(())
                            case 2 => i(QVec(v.prop * 0.5, v.v)) + step(())
                            case -4 => i(QVec(v.prop * -0.25, v.v)) + step(())
                            case 4 => i(QVec(v.prop * 0.25, v.v)) + step(())
                            case -8 => i(QVec(v.prop * -0.125, v.v)) + step(())
                            case 8 => i(QVec(v.prop * 0.125, v.v)) + step(())
                            case -16 => i(QVec(v.prop * -0.0625, v.v)) + step(())
                            case 16 => i(QVec(v.prop * 0.0625, v.v)) + step(())
                        }
                    }else{
                        td match{
                            case -2 => e(QVec(v.prop * 1/0.5, v.v), Circuit(gates.tail)) + step(())
                            case 2 => e(QVec(v.prop * 0.5, v.v), Circuit(gates.tail)) + step(())
                            case -4 => e(QVec(v.prop * 1/0.25, v.v), Circuit(gates.tail)) + step(())
                            case 4 => e(QVec(v.prop * 0.25, v.v), Circuit(gates.tail)) + step(())
                            case -8 => e(QVec(v.prop * 1/0.125, v.v), Circuit(gates.tail)) + step(())
                            case 8 => e(QVec(v.prop * 0.125, v.v), Circuit(gates.tail)) + step(())
                            case -16 => e(QVec(v.prop * 1/0.0625, v.v), Circuit(gates.tail)) + step(())
                            case 16 => e(QVec(v.prop * 0.0625, v.v), Circuit(gates.tail)) + step(())
                        }
                    }
                }else{
                    if(fin){
                        i(v) + step(())
                    }else{ e(v, Circuit(gates.tail)) + step(())}
                }
            case ModGate(a, n) =>
                val rv: Vector[Boolean] = v.v.slice(0, 4)
                val relevantbits = rv.map(a => if(a){1}else{0})
                val rbstring = relevantbits.mkString("")
                val in = Integer.parseInt(rbstring, 2)
                val r = Math.floor(Math.pow(a, in) % n).toInt
                val l = r.toBinaryString.toList.map(a => if(a == '1'){true}else{false})
                if(fin){
                    i(QVec(v.prop, rv ++ l)) + step(())
                }else{
                    e(QVec(v.prop, rv ++ l), Circuit(gates.tail)) + step(())
                }
        }
    

    def scaleAndSample() = {
        val x = e.logSoup.split("Molecules: ")(1)
        val each = x.split("r/P\\(QVec\\(")
        val pps = each.map(s => {
        val r = s.split(",\\)\\) * ")(0)
        val p = r.split("\\+")(0).drop(1)
        val v = r.split(",Vector").filter(u => u.contains("true") || u.contains("false"))
        (p, v.toVector)
        }).drop(1)

        val sqred = pps.map(x => (x._1.toDouble * x._1.toDouble, x._2)).toList
        val tot = sqred.foldLeft(0d)((a, b) => a + b._1)
        val scale = 1 / tot
        val scaled = pps.map(x => {
            val castToBool = x._2.map(b => b == "true")
            (x._1.toDouble * scale, castToBool)
        })
        new BasicSampler(scaled.toList).sample()
    }

    
    // Example Simon Circuit 
    e((QVec(1d, Vector(false, false, false, false)), Circuit(List(H(0), H(1), CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), H(0), H(1))))) + step(()) + step(())
}