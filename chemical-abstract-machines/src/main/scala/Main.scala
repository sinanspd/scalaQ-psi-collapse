package example2 

import io.chymyst.jc._
import org.slf4j.LoggerFactory
import scala.util.Random
import scala.math._
import spire.math._
import spire.implicits._ 
import fs2._
import cats.effect.IO
import scala.util.control.NoStackTrace
import cats.Eval
import cats.effect.unsafe.implicits.global
import com.sinanspd.qure.circuit._
import com.sinanspd.qure.circuit.gates._
import com.sinanspd.qure.circuit.circuitError._
import com.sinanspd.qure.circuit.sampler._

class FakeSimonSampler(c: Circuit) extends Sampler {
        val s = getRandomStateSpace()

        private def getRandomStateSpace() = {
            val numberOfH = c.remainingGates.collect { case a : H => a }.length
            val s = gen(2, List()).filter(l => l.forall(_ == l.head))
            s(new Random().nextInt(s.length))
        }

        private def gen(n: Int, acc: List[Vector[Boolean]]) : List[Vector[Boolean]] = {
            if (n == 0) {
                acc
            } else {
                if (acc.length == 0) {
                gen(n - 1, List(Vector(false), Vector(true)))
                } else {
                gen(n - 1, (for (i <- acc) yield i :+ false) ++ (for (j <- acc) yield j :+ true))
                }
            }
        }

        def sample() = s
}

object Main2 extends App {

    val correctAnswers = List() // Fill this out before running 
    val concurrentInstances = 2 // Fill this out before running 
    
    val logger = LoggerFactory.getLogger("example.Main")
    val hScale : Double = 1.0 / Math.sqrt(2.0)
    val decider = new Random()

    val r = m[QVec]

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
        go { case r(a) + r(b) ⇒ {
                logger.debug(s"Possible Reaction between $a and $b"); 
                if(a.v.sameElements(b.v)){ 
                    logger.debug("Molecules compatible. Reaction starting")
                    val newQ = QVec(a.prop + b.prop, a.v)
                    if(newQ.prop.real.abs >= 0.6){
                        logger.info(s"Result $newQ")
                    }else if(newQ.prop.real.abs == 0d){
                        logger.debug("Destrictive Interference")
                    }else{
                        logger.debug(s"Reaction finished, releasesing $newQ into the solution")
                        r(newQ)
                    }
                }else{
                    logger.debug("Incompatible molecules. Returning molecules to the pool")
                    r(a) + r(b)
                }
            }
        }
    )

    def build(c: Circuit, v: QVec)(implicit sampler: Sampler) : Unit = {
        c.remainingGates match{ 
            case Nil => 
                val midresult = sampler.sample()
                if(v.v.takeRight(v.v.length / 2).sameElements(midresult)){
                    val newMolecule = QVec(v.prop, v.v.take(v.v.length / 2))
                    logger.debug("Releasing " + newMolecule)
                    r(newMolecule)
                }
            case x :: xs => 
                x match {
                    case H(t) => 
                        val sign = if (v.v(t)) -1 else 1
                        val h1 = QVec(sign * hScale * v.prop, v.v)
                        val h2 =  QVec(hScale * v.prop, v.v.updated(t, !v.v(t)))
                        build(Circuit(xs), h1)
                        build(Circuit(xs), h2)
                    case CX(c, t) => 
                        if(v.v(c)){
                            build(Circuit(xs), QVec(v.prop, v.v.updated(t, !(v.v(t)))))
                        }else{
                            build(Circuit(xs), QVec(v.prop, v.v))
                        }
                    case Swap(q1, q2) => 
                         val source = v.v(q1)
                         val target = v.v(q2)
                         val firstOverwrite = v.v.updated(q2, source)
                         val second = firstOverwrite.updated(q1, target)
                         build(Circuit(xs), QVec(v.prop, second))
                    case Measure(q) => ??? 
                }
        }
    }

    //shor filter based on 0001 
    // a^x mod n

    val fourbitsimon = Circuit(List(H(0), H(1), CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), H(0), H(1)))
    val excs = QVec(1d, Vector(false, false, false, false))

    implicit val sampler = new FakeSimonSampler(fourbitsimon)

    Stream.eval(IO{build(fourbitsimon, excs)}).repeatN(concurrentInstances).compile.toVector.unsafeRunSync()

    //shores 
    //grovers 

}