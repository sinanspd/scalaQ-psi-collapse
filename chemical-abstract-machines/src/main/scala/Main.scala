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

/// This implementation uses streaming to concurrently evaluate the circuit, then falls back
// to CHAM to built interference. More efficient than the fully CHAM implementation 

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

class FakeShorSampler() extends Sampler{ 
    def sample() = Vector(true, false, false, false) //doesn't matter what this returns 
}

case class ModGate(a: Int, n: Int) extends Gate 

object Main2 extends App {

    val correctAnswers = List()  
    val concurrentInstances = 4 // Fill this out before running 
    
    val logger = LoggerFactory.getLogger("example.Main")
    val hScale : Double = 1.0 / Math.sqrt(2.0)
    val decider = new Random()

    val r = m[QVec]

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
        go { case r(a) + r(b) ⇒ {
                logger.debug(s"Possible Reaction between $a and $b"); 
                if(a.v.sameElements(b.v)){ 
                    logger.debug("Molecules compatible. Reaction starting")
                    val newQ = QVec(a.prop + b.prop, a.v)
                    if(newQ.prop.real.abs >= 0.9){
                        if(!med){
                            logger.info(s"Result $newQ")
                            med = true
                            scaleAndSample()
                        }
                        
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
               // val midresult = sampler.sample()
                // if(v.v.takeRight(v.v.length / 2).sameElements(midresult)){
                //     val newMolecule = QVec(v.prop, v.v.take(v.v.length / 2))
                //     logger.debug("Releasing " + newMolecule)
                //     r(newMolecule)
                // }
                val take = 4
                val newMolecule = QVec(v.prop, v.v.take(take))
                logger.debug("Releasing " + newMolecule)
                r(newMolecule)
            case x :: xs => 
                x match {
                    case X(t) => 
                        val nv = QVec(v.prop, v.v.updated(t, !v.v(t)))
                        build(Circuit(xs), nv)
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
                        if(target){
                            build(Circuit(xs), QVec(v.prop * new Complex(0d, 1d) * coeff , v.v))
                        }else{
                            build(Circuit(xs), QVec(v.prop * -1 * new Complex(0d, 1d) * coeff, v.v))
                        }
                    case Rotate(td, q) => 
                        td match{
                            case -2 => build(Circuit(xs), QVec(v.prop * 1/0.5, v.v))
                            case 2 => build(Circuit(xs), QVec(v.prop * 0.5, v.v))
                            case -4 => build(Circuit(xs), QVec(v.prop * 1/0.25, v.v))
                            case 4 => build(Circuit(xs), QVec(v.prop * 0.25, v.v))
                            case -8 => build(Circuit(xs), QVec(v.prop * 1/0.125, v.v))
                            case 8 => build(Circuit(xs), QVec(v.prop * 0.125, v.v))
                            case -16 => build(Circuit(xs), QVec(v.prop * 1/0.0625, v.v))
                            case 16 => build(Circuit(xs), QVec(v.prop * 0.0625, v.v))
                        }
                    case CRotate(c, td, q) => 
                        val target = v.v(c) 
                        if(target == true && v.v(q) == true){
                            td match{
                                case -2 => build(Circuit(xs), QVec(v.prop * 1/0.5, v.v))
                                case 2 => build(Circuit(xs), QVec(v.prop * 0.5, v.v))
                                case -4 => build(Circuit(xs), QVec(v.prop * 1/0.25, v.v))
                                case 4 => build(Circuit(xs), QVec(v.prop * 0.25, v.v))
                                case -8 => build(Circuit(xs), QVec(v.prop * 1/0.125, v.v))
                                case 8 => build(Circuit(xs), QVec(v.prop * 0.125, v.v))
                                case -16 => build(Circuit(xs), QVec(v.prop * 1/0.0625, v.v))
                                case 16 => build(Circuit(xs), QVec(v.prop * 0.0625, v.v))
                            }
                        }else{
                            build(Circuit(xs), v)
                        }
                
                    case ModGate(a, n) =>
                        val rv: Vector[Boolean] = v.v.slice(0, 4)
                        val relevantbits = rv.map(a => if(a){1}else{0})
                        val rbstring = relevantbits.mkString("")
                        val i = Integer.parseInt(rbstring, 2)
                        val r = Math.floor(Math.pow(a, i) % n).toInt
                        val l = r.toBinaryString.toList.map(a => if(a == '1'){true}else{false})
                        build(Circuit(xs), QVec(v.prop, rv ++ l)) 
                    case Measure(q) => ??? 
                }
        }
    }

    def scaleAndSample() = {
        val x = r.logSoup.split("Molecules: ")(1)
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
    val fourbitsimon = Circuit(List(H(0), H(1), CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), H(0), H(1)))
    implicit val sampler = new FakeSimonSampler(fourbitsimon)
    val excs = QVec(1d, Vector(false, false, false, false))
    Stream.eval(IO{build(fourbitsimon, excs)}).repeatN(concurrentInstances).compile.toVector.unsafeRunSync()


    // Example Shor Circuit
    //implicit val sampler = new FakeShorSampler()
    val _15bitshor = QVec(1d, Vector(false, false, false ,false ,false, false, false, false)) 
    val n15shor = Circuit(List(H(0), H(1), H(2), H(3), X(0), X(1), X(2), X(3), 
    ModGate(2, 15), 
    H(0), CRotate(0, -2, 1), Swap(0, 1), CRotate(1, -4, 2), Swap(1, 2),
    H(1), CRotate(2, -8, 3), Swap(2, 3), CRotate(1, -2, 2), Swap(1, 2), CRotate(2, -4, 3),
    Swap(2, 3), H(2), CRotate(2, -2, 3), Swap(2, 3), H(3)
    ))

    val _shor21 = QVec(1d, Vector(false, false, false, false, false, false, false, false))
    val n21shor = Circuit(List(H(0), H(1), H(2), CX(1, 5), H(3), CX(1, 7), CX(2, 5), CX(0, 7),
    CX(5, 3), RZ(-4, 3), CX(7, 3), RZ(4, 3), CX(5, 3), RZ(-4, 3), RZ(4, 5), CX(7, 3), CX(7, 5), RZ(4, 3), RZ(-4, 5), RZ(4, 7), 
    H(3), CX(7, 5), CX(0, 7), CX(1, 7), H(7), CX(3, 7), RZ(-4, 7), CX(0, 7), RZ(4,7), CX(3, 7), 
    RZ(4, 3), RZ(-4, 7), CX(0, 7), CX(0, 3), RZ(4, 7), RZ(4, 0), RZ(-4, 3), H(7), CX(0, 3), CX(3, 5), CX(7, 5), CX(0, 5), X(7), CX(2, 7), CX(1, 7), CX(0, 7),
    CX(0, 2), CX(2, 0), CX(0, 2), H(0), RZ(-4, 0), CX(0, 1), RZ(4, 1), CX(0, 1), RZ(-8, 0), RZ(-4, 1), CX(0, 2), H(1), RZ(8, 2), RZ(-4, 1), CX(0,2),
    RZ(-8, 2), CX(1, 2), RZ(4, 2), CX(1, 2), RZ(-4, 2), H(2)))

    //Example Grover Circuit 
    val _g0000 = QVec(1d, Vector(false, false, false, false))
    val grover0000 = Circuit(List(
        H(0), H(1), H(2), H(3), X(0), X(1), X(2), X(3),
        CRotate(0, 4, 3), CX(0, 1), CRotate(1, -4, 3), CX(0, 1), CRotate(1, 4, 3), CX(1, 2), CRotate(2, -4, 3), CX(0, 2), CRotate(2, 4, 3), CX(1, 2), CRotate(2, -4, 3), CX(0, 2), CRotate(2, 4, 3),
        X(1), X(0), X(2), X(3), H(0), H(1), H(2), H(3), X(1), X(0), X(2), X(3),
        CRotate(0, 4, 3), CX(0, 1), CRotate(1, -4, 3), CX(0, 1), CRotate(1, 4, 3), CX(1,2), CRotate(2, -4, 3), CX(0, 2), CRotate(2, 4 , 3), CX(1, 2), CRotate(2, -4, 3), CX(0, 2), CRotate(2, 4, 3),
        X(1), X(0), X(2), X(3), H(0), H(1), H(2), H(3)))

    val groverSAT011 = Circuit(List(H(0), H(1), H(2), X(2), CX(1, 2), Rotate(-4, 2), CX(0, 2), Rotate(4, 2), CX(1, 2),
    Rotate(4, 1), Rotate(-4, 2), CX(0, 2), CX(0, 1), Rotate(4, 2), CX(0,1), Rotate(4, 2), Rotate(4, 0), Rotate(-4, 1), X(2), CX(0,1),
    H(0), H(1), H(2), X(0), X(1), X(2),
    CX(1,2), Rotate(-4, 2), CX(0,2), Rotate(4, 2), CX(1, 2), Rotate(4, 1), Rotate(-4, 2), CX(0, 2), CX(0, 1), 
    Rotate(4, 0), Rotate(-4, 1), Rotate(4, 1), X(2), H(2), CX(0, 1), X(0), X(1), X(2), H(0), H(1),
    CX(1, 2), Rotate(-4, 2), CX(0, 2), Rotate(4, 2), CX(1,2), Rotate(4, 1), Rotate(-4, 2), CX(0, 2), CX(0, 1), 
    Rotate(4, 0), Rotate(-4, 1), Rotate(4, 2), X(0), CX(0, 1),
    H(0), H(1), H(2), X(0), X(1), X(2), H(0), H(1), H(2)))

    val _gSAT001 = QVec(1d, Vector(false, false, false))
}