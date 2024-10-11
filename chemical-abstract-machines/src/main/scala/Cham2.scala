package example2 

import io.chymyst.jc._
import org.slf4j.LoggerFactory
import scala.util.Random
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
import spire.math._
import spire.implicits._ 

//Fully Cham implementation 
object Cham2 extends App {

    val correctAnswers = List() 
    val concurrentInstances = 5 // Fill this out before running 
    
    val logger = LoggerFactory.getLogger("example2.Cham2")
    val hScale : Double = 1.0 / Math.sqrt(2.0)
    val decider = new Random()

    val terminate = m[QVec]
    val commit = m[(QVec, Circuit, String)]
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
        go { case terminate(a) + terminate(b) ⇒ { //two intermediate results ready to interfere 
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
                        terminate(newQ)
                    }
                }else{
                    logger.debug("Incompatible molecules. Returning molecules to the pool")
                    terminate(a) + terminate(b)
                }
            }
        },
        go { //local progress 
            case commit(a) + step(()) ⇒ {
                if(a._2.remainingGates.length == 1){ //whatever we release here will be ready for interference
                    applyGate(a._2.remainingGates, a._1, true, a._3)
                }else{
                    applyGate(a._2.remainingGates, a._1, false, a._3)
                }
            }
        },
        go {
            case step(()) + step(()) ⇒ step(()) + step(())
        }
    )


    def fx(){
        Complex(1d, 0) * hScale
    }
    
    def applyGate(gates: List[Gate], v: QVec, fin: Boolean, world: String) =  // we need to re-release a step() every time otherwise there wont be enough in the pool 
        gates.head match {
            case _ @ X(t) => {
                val nv = QVec(v.prop, v.v.updated(t, !v.v(t)))
                if(fin){
                    println(s"Done after X: $world, with $nv")
                    terminate(nv) + step(())
                }else{
                    println(s"Applied X: $world, \n now: $nv")
                    commit((nv, Circuit(gates.tail), world)) + step(())
                }
            }
            case _ @ H(t) => {
                val sign = if (v.v(t)) {-1} else {1}
                val ll = Complex(1d, 0)
                if(fin){    
                    println(s"Done with H: $world")
                    val nc1 = Complex(sign * hScale * v.prop.real, v.prop.imag)
                    val nc2 = Complex(hScale * v.prop.real, v.prop.imag)
                    terminate(QVec(nc1, v.v)) + terminate(QVec(nc2, v.v.updated(t, !v.v(t)))) + step(())
                }else{
                    val nc1 = Complex(sign * hScale * v.prop.real, v.prop.imag)
                    val nc2 = Complex(hScale * v.prop.real, v.prop.imag)
                    val qv1 = QVec(nc2, v.v)
                    val qv2 = QVec(nc2, v.v.updated(t, !v.v(t)))
                    val w1n = Random.nextInt()
                    val w2n = Random.nextInt()
                    println(s"Applied H: World $w1n: $qv1, World $w2n: $qv2")
                    commit((qv1, Circuit(gates.tail), "World " + w1n)) + commit((qv2, Circuit(gates.tail), "World " + w2n)) + step(()) + step(())
                }
            }
            case CX(c, t) => 
                if(fin){
                    if(v.v(c)){
                        println(s"Done after CX: $world, with ${QVec(v.prop, v.v.updated(t, !(v.v(t))))}")
                        terminate(QVec(v.prop, v.v.updated(t, !(v.v(t))))) + step(())
                    }else{
                        println(s"Done after CX: $world, with ${QVec(v.prop, v.v)}")
                        terminate(QVec(v.prop, v.v)) + step(())
                    }
                }else{
                    if(v.v(c)){
                        val nv = QVec(v.prop, v.v.updated(t, !(v.v(t))))
                        println(s"Applied CX: $world, \n now: $nv")
                        commit((nv, Circuit(gates.tail), world)) + step(())
                    }else{
                        val nv = QVec(v.prop, v.v)
                        println(s"Applied CX: $world, \n now: $nv")
                        commit((nv, Circuit(gates.tail), world)) + step(())
                    }
                }
            case Swap(q1, q2) => 
                val source = v.v(q1)
                val target = v.v(q2)
                val firstOverwrite = v.v.updated(q2, source)
                val second = firstOverwrite.updated(q1, target)
                if(fin){
                    println(s"Done after Swap: $world, with ${QVec(v.prop, second)}")
                    terminate(QVec(v.prop, second)) + step(())
                }else{
                    println(s"Applied Swap: $world, \n now: ${QVec(v.prop, second)}")
                    commit((QVec(v.prop, second), Circuit(gates.tail), world)) + step(())    
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
                        println(s"Done after RZ: $world, with ${QVec(v.prop * new Complex(0d, 1d) * coeff , v.v)}")
                        terminate(QVec(v.prop * new Complex(0d, 1d) * coeff , v.v)) + step(())
                    }else{
                        println(s"Done after RZ: $world, with ${QVec(v.prop * -1 * new Complex(0d, 1d) * coeff, v.v)}")
                        terminate(QVec(v.prop * -1 * new Complex(0d, 1d) * coeff, v.v)) + step(())
                    }
                }else{
                    if(target){
                        println(s"Applied RZ: $world, \n now: ${QVec(v.prop * new Complex(0d, 1d) * coeff , v.v)}")
                        commit(QVec(v.prop * new Complex(0d, 1d) * coeff , v.v) , Circuit(gates.tail), world) + step(())
                    }else{
                        println(s"Applied RZ: $world, \n now: ${QVec(v.prop * -1 * new Complex(0d, 1d) * coeff, v.v)}")
                        commit(QVec(v.prop * -1 * new Complex(0d, 1d) * coeff, v.v), Circuit(gates.tail), world) + step(())
                    }
                }
            case Rotate(td, q) => 
                if(fin){
                    td match{
                        case -2 => terminate(QVec(v.prop * 1/0.5, v.v)) + step(())
                        case 2 => terminate(QVec(v.prop * 0.5, v.v)) + step(())
                        case -4 => terminate(QVec(v.prop * 1/0.25, v.v)) + step(())
                        case 4 => terminate(QVec(v.prop * 0.25, v.v)) + step(())
                        case -8 => terminate(QVec(v.prop * 1/0.125, v.v)) + step(())
                        case 8 => terminate(QVec(v.prop * 0.125, v.v)) + step(())
                        case -16 => terminate(QVec(v.prop * 1/0.0625, v.v)) + step(())
                        case 16 => terminate(QVec(v.prop * 0.0625, v.v)) + step(())
                    }
                }else{
                    td match{
                        case -2 => commit(QVec(v.prop * 1/0.5, v.v), Circuit(gates.tail), world) + step(())
                        case 2 => commit(QVec(v.prop * 0.5, v.v), Circuit(gates.tail), world) + step(()) 
                        case -4 => commit(QVec(v.prop * 1/0.25, v.v), Circuit(gates.tail), world) + step(())
                        case 4 => commit(QVec(v.prop * 0.25, v.v), Circuit(gates.tail), world) + step(()) 
                        case -8 => commit(QVec(v.prop * 1/0.125, v.v), Circuit(gates.tail), world) + step(())
                        case 8 => commit(QVec(v.prop * 0.125, v.v), Circuit(gates.tail), world) + step(())
                        case -16 => commit(QVec(v.prop * 1/0.0625, v.v), Circuit(gates.tail), world) + step(())
                        case 16 => commit(QVec(v.prop * 0.0625, v.v), Circuit(gates.tail), world) + step(())
                    }
                }
            case CRotate(c, td, q) => 
                println(s"Applying CR gate in $world")
                val target = v.v(c) 
                if(target == true && v.v(q) == true){
                    if(fin){
                        td match{
                            case -2 => terminate(QVec(v.prop * -0.5, v.v)) + step(())
                            case 2 => terminate(QVec(v.prop * 0.5, v.v)) + step(())
                            case -4 => terminate(QVec(v.prop * -0.25, v.v)) + step(())
                            case 4 => terminate(QVec(v.prop * 0.25, v.v)) + step(())
                            case -8 => terminate(QVec(v.prop * -0.125, v.v)) + step(())
                            case 8 => terminate(QVec(v.prop * 0.125, v.v)) + step(())
                            case -16 => terminate(QVec(v.prop * -0.0625, v.v)) + step(())
                            case 16 => terminate(QVec(v.prop * 0.0625, v.v)) + step(())
                        }
                    }else{
                        td match{
                            case -2 => commit(QVec(v.prop * 1/0.5, v.v), Circuit(gates.tail), world) + step(())
                            case 2 => commit(QVec(v.prop * 0.5, v.v), Circuit(gates.tail), world) + step(())
                            case -4 => commit(QVec(v.prop * 1/0.25, v.v), Circuit(gates.tail), world) + step(())
                            case 4 => commit(QVec(v.prop * 0.25, v.v), Circuit(gates.tail), world) + step(())
                            case -8 => commit(QVec(v.prop * 1/0.125, v.v), Circuit(gates.tail), world) + step(())
                            case 8 => commit(QVec(v.prop * 0.125, v.v), Circuit(gates.tail), world) + step(())
                            case -16 => commit(QVec(v.prop * 1/0.0625, v.v), Circuit(gates.tail), world) + step(())
                            case 16 => commit(QVec(v.prop * 0.0625, v.v), Circuit(gates.tail), world) + step(())
                        }
                    }
                }else{
                    if(fin){
                        terminate(v) + step(())
                    }else{ commit(v, Circuit(gates.tail), world) + step(())}
                }
            case ModGate(a, n) =>
                val rv: Vector[Boolean] = v.v.slice(0, 4)
                val relevantbits = rv.map(a => if(a){1}else{0})
                val rbstring = relevantbits.mkString("")
                val in = Integer.parseInt(rbstring, 2)
                val r = Math.floor(Math.pow(a, in) % n).toInt
                val l = r.toBinaryString.toList.map(a => if(a == '1'){true}else{false})
                if(fin){
                    terminate(QVec(v.prop, rv ++ l)) + step(())
                }else{
                    commit(QVec(v.prop, rv ++ l), Circuit(gates.tail), world) + step(())
                }
            case _ => println("Something Went Wrong, I Don't Recognize The Gate")
        }
    

    def scaleAndSample() = {
        val x = commit.logSoup.split("Molecules: ")(1)
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


    val zz = QVec(Complex(1d, 0), Vector(false, false, false, false))
    ///applyGate(List(H(0), X(0), CX(0,2), CX(1,2), CX(1,3)), zz, false, "World 0")
    // Example Simon Circuit 
    commit((QVec(Complex(1d, 0), Vector(false, false, false, false)), Circuit(List(H(0), X(0), CX(0,2), CX(1,2), CX(1,3))), "World 0")) + step(())
    //e((QVec(1d, Vector(false, false, false, false)), Circuit(List(H(0), H(1), CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), H(0), H(1))))) + step(()) + step(())
}