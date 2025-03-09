package com.sinanspd 

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

    val logger = LoggerFactory.getLogger("com.sinanspd.Cham2")
    val concurrentInstances = 3 //10
    val hScale : Double = 1.0 / Math.sqrt(2.0)
    val decider = new Random()
    val correctAnswers = List() 
    val tp = BlockingPool(32) 
    val terminateEarly = true

    //Molecules in the soup
    val terminate = m[QVec]
    //Intermediadery Circuit Molecules
    val commit = m[(QVec, Circuit, String)]
    // Apply Game 
    val step = m[Unit]
    // Use if you want to bake result back into the reaction 
    val result = b[Unit, String] 
   
    @volatile
    var med = false
    // We can try volatile variables to keep track of mols (https://github.com/Chymyst/chymyst-core/blob/master/docs/chymyst09.md)
    @volatile var mols = List()
    
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
    case class ModGate(a: Int, n: Int) extends Gate 


    site(tp) (
        go { case terminate(a) + terminate(b) ⇒ { //two intermediate results ready to interfere 
                //logger.debug(s"Possible Reaction between $a and $b"); 
                if(a.v.sameElements(b.v)){ 
                    logger.info(s"Molecules compatible. Reaction starting $a, $b")
                    val newQ = QVec(Complex(a.prop.real + b.prop.real, a.prop.imag + b.prop.imag), a.v)
                    if(newQ.prop.real.abs >= 0.5){ //set threshold here 
                        if(!med){
                            logger.info(s"Threshold Passed")
                            //med = true
                            terminate(newQ)
                            if(!terminateEarly){
                               scaleAndSample()
                            }else{
                             println(s"---------------- RESULT -------- \n $newQ \n -------------------")
                               tp.shutdownNow() 
                            }
                        }
                    }else if(newQ.prop.real.abs == 0d){
                        logger.debug("Destructive Interference")
                    }else{
                        logger.debug(s"Reaction finished, releasesing $newQ into the solution")
                        terminate(newQ)
                    }
                }else{
                    //logger.debug("Incompatible molecules. Returning molecules to the pool")
                    terminate(a) + terminate(b)
                }
            }
        }
    )
    site(
        go { //local progress 
            case commit(a) + step(()) ⇒ { // I can specify thread pool here too (tp per reaction site)
                if(a._2.remainingGates.length == 1){ //whatever we release here will be ready for interference
                    Thread.sleep(scala.util.Random.nextInt(40))
                    applyGate(a._2.remainingGates, a._1, true, a._3)
                }else if(a._2.remainingGates.length > 1){
                    applyGate(a._2.remainingGates, a._1, false, a._3)
                }
            }
            
        }
    )
    
    def applyGate(gates: List[Gate], v: QVec, fin: Boolean, world: String) =  // we need to re-release a step() every time otherwise there wont be enough in the pool 
        gates.head match {
            case _ @ X(t) => {
                val nv = QVec(v.prop, v.v.updated(t, !v.v(t)))
                if(fin){
                    logger.debug(s"Done after X: $world, with $nv")
                    terminate(nv) + step(())
                }else{
                    logger.debug(s"Applied X: $world, \n now: $nv, remaining: ${gates.tail}")
                    commit((nv, Circuit(gates.tail), world)) + step(())
                }
            }
            case _ @ H(t) => {
                val sign = if (v.v(t)) {-1} else {1}
                val ll = Complex(1d, 0)
                if(fin){   
                    val nc1 = Complex(sign * hScale * v.prop.real, sign * hScale * v.prop.imag)
                    val nc2 = Complex(hScale * v.prop.real, hScale * v.prop.imag) 
                    val m1 = if(v.v(t)){QVec(nc1, v.v)}else{QVec(nc1, v.v.updated(t, !v.v(t)))}
                    val m2 = if(v.v(t)){QVec(nc2, v.v.updated(t, !v.v(t)))}else{QVec(nc2, v.v)}

                    // Uncomment the following 2 lines if you aren't running Simon
                    logger.info(s"Done with H: $world, m1: $m1, m2: $m2")
                    terminate(m1) + terminate(m2) + step(()) + step(())
                    
                    // Uncomment the following block if you are running Simon
                    // This "fakes" the last half of the qubits being measured 
                    // logger.debug(s"----------- Meas: $middleValue ----- Mine: $m1 ------- $m2")
                    // if(m1.v.takeRight(v.v.length / 2).sameElements(middleValue)){
                    //      val newMolecule = QVec(m1.prop, m1.v.take(v.v.length / 2))
                    //      logger.debug(s"Done with H: $world, m1 is truncated: $newMolecule")
                    //      terminate(newMolecule)
                    //  }
                    //  if(m2.v.takeRight(v.v.length / 2).sameElements(middleValue)){
                    //      val newMolecule = QVec(m2.prop, m2.v.take(v.v.length / 2))
                    //      logger.debug(s"Done with H: $world, m2 is truncated: $newMolecule")
                    //      terminate(newMolecule)
                    //  }
                }else{
                    val nc1 = Complex(sign * hScale * v.prop.real, sign * hScale * v.prop.imag)
                    val nc2 = Complex(hScale * v.prop.real, hScale * v.prop.imag)
                    val m1 = if(v.v(t)){QVec(nc1, v.v)}else{QVec(nc1, v.v.updated(t, !v.v(t)))}
                    val m2 = if(v.v(t)){QVec(nc2, v.v.updated(t, !v.v(t)))}else{QVec(nc2, v.v)}
                    val w1n = Random.nextInt()
                    val w2n = Random.nextInt()
                    logger.debug(s"Started with ${v.v} Applied H: World $w1n: $m1, World $w2n: $m2")
                    commit((m1, Circuit(gates.tail), "World " + w1n)) + commit((m2, Circuit(gates.tail), "World " + w2n)) + step(()) + step(())
                }
            }
            case CX(c, t) => 
                if(fin){
                    if(v.v(c)){
                        logger.debug(s"Done after CX: $world, with ${QVec(v.prop, v.v.updated(t, !(v.v(t))))}")
                        terminate(QVec(v.prop, v.v.updated(t, !(v.v(t))))) + step(())
                    }else{
                        logger.debug(s"Done after CX: $world, with ${QVec(v.prop, v.v)}")
                        terminate(QVec(v.prop, v.v)) + step(())
                    }
                }else{
                    if(v.v(c)){
                        val nv = QVec(v.prop, v.v.updated(t, !(v.v(t))))
                        logger.debug(s"Applied CX: $world, \n now: $nv")
                        commit((nv, Circuit(gates.tail), world)) + step(())
                    }else{
                        val nv = QVec(v.prop, v.v)
                        logger.debug(s"Applied CX: $world, \n now: $nv")
                        commit((nv, Circuit(gates.tail), world)) + step(())
                    }
                }
            case Swap(q1, q2) => 
                val source = v.v(q1)
                val target = v.v(q2)
                val firstOverwrite = v.v.updated(q2, source)
                val second = firstOverwrite.updated(q1, target)
                if(fin){
                    logger.debug(s"Done after Swap: $world, with ${QVec(v.prop, second)}")
                    terminate(QVec(v.prop, second)) + step(())
                }else{
                    logger.debug(s"Applied Swap: $world, \n now: ${QVec(v.prop, second)}")
                    commit((QVec(v.prop, second), Circuit(gates.tail), world)) + step(())    
                }
            case RZ(td, q) => 
                val target = v.v(q)
                val coefft = (c : Int) => c match{ //rough estimates are fine, estimates e^{+/- i theta / 2 } where theta = pi/n. We are passed n
                    case -2 => Complex(0.7071, -0.7071) // e^{-i pi / 4}
                    case 2 => Complex(0.7071, 0.7071) // e^{i pi / 4}
                    case -4 => Complex(0.924, -0.383) // e^{-i pi / 8}
                    case 4 => Complex(0.924, 0.383)  // e^{i pi / 8}
                    case -8 => Complex(0.9808, -0.1951) // e^{-i pi / 16}
                    case 8 => Complex(0.9808, 0.1951) // e^{i pi / 16}
                    case -16 => Complex(0.9952, -0.0980) // e^{-i pi / 32}
                    case 16 => Complex(0.9952, 0.0980) // e^{-i pi / 32}
                }
                if(fin){
                    if(target){
                        val coeff = coefft(td)
                        val ph = new Complex((v.prop.real * coeff.real) - (v.prop.imag * coeff.imag), (v.prop.real * coeff.imag) + (v.prop.imag * coeff.real))
                        logger.debug(s"Done after RZ: $world, with ${QVec(ph , v.v)}")
                        terminate(QVec(ph, v.v)) + step(())
                    }else{
                        val coeff = coefft(-1 * td)
                        val ph = new Complex((v.prop.real * coeff.real) - (v.prop.imag * coeff.imag), (v.prop.real * coeff.imag) + (v.prop.imag * coeff.real))
                        logger.debug(s"Done after RZ: $world, with ${QVec(ph, v.v)}")
                        terminate(QVec(ph, v.v)) + step(())
                    }
                }else{
                    if(target){
                        val coeff = coefft(td)
                        val ph = new Complex((v.prop.real * coeff.real) - (v.prop.imag * coeff.imag), (v.prop.real * coeff.imag) + (v.prop.imag * coeff.real))
                        logger.debug(s"Applied RZ: $world, \n now: ${QVec(ph, v.v)}")
                        commit(QVec(ph , v.v) , Circuit(gates.tail), world) + step(())
                    }else{
                        val coeff = coefft(-1 * td)
                        val ph = new Complex((v.prop.real * coeff.real) - (v.prop.imag * coeff.imag), (v.prop.real * coeff.imag) + (v.prop.imag * coeff.real))
                        logger.debug(s"Applied RZ: $world, \n now: ${QVec(ph, v.v)}")
                        commit(QVec(ph, v.v), Circuit(gates.tail), world) + step(())
                    }
                }
            // case Rotate(td, q) => //RX
            //     if(fin){
            //         td match{
            //             case -2 => terminate(QVec(v.prop * 1/0.5, v.v)) + step(())
            //             case 2 => terminate(QVec(v.prop * 0.5, v.v)) + step(())
            //             case -4 => terminate(QVec(v.prop * 1/0.25, v.v)) + step(())
            //             case 4 => terminate(QVec(v.prop * 0.25, v.v)) + step(())
            //             case -8 => terminate(QVec(v.prop * 1/0.125, v.v)) + step(())
            //             case 8 => terminate(QVec(v.prop * 0.125, v.v)) + step(())
            //             case -16 => terminate(QVec(v.prop * 1/0.0625, v.v)) + step(())
            //             case 16 => terminate(QVec(v.prop * 0.0625, v.v)) + step(())
            //         }
                // }else{
                //     td match{
                //         case -2 => commit(QVec(v.prop * 1/0.5, v.v), Circuit(gates.tail), world) + step(())
                //         case 2 => commit(QVec(v.prop * 0.5, v.v), Circuit(gates.tail), world) + step(()) 
                //         case -4 => commit(QVec(v.prop * 1/0.25, v.v), Circuit(gates.tail), world) + step(())
                //         case 4 => commit(QVec(v.prop * 0.25, v.v), Circuit(gates.tail), world) + step(()) 
                //         case -8 => commit(QVec(v.prop * 1/0.125, v.v), Circuit(gates.tail), world) + step(())
                //         case 8 => commit(QVec(v.prop * 0.125, v.v), Circuit(gates.tail), world) + step(())
                //         case -16 => commit(QVec(v.prop * 1/0.0625, v.v), Circuit(gates.tail), world) + step(())
                //         case 16 => commit(QVec(v.prop * 0.0625, v.v), Circuit(gates.tail), world) + step(())
                //     }
                // }
            case CRotate(c, td, q) =>  
                val target = v.v(c) 
                val coefft = (a : Int) => a match{ //rough estimates are fine, estimates e^{+/- i theta / 2 } where theta = pi/n. We are passed n
                    case -2 => Complex(0.7071, -0.7071) // e^{-i pi / 4}
                    case 2 => Complex(0.7071, 0.7071) // e^{i pi / 4}
                    case -4 => Complex(0.924, -0.383) // e^{-i pi / 8}
                    case 4 => Complex(0.924, 0.383)  // e^{i pi / 8}
                    case -8 => Complex(0.9808, -0.1951) // e^{-i pi / 16}
                    case 8 => Complex(0.9808, 0.1951) // e^{i pi / 16}
                    case -16 => Complex(0.9952, -0.0980) // e^{-i pi / 32}
                    case 16 => Complex(0.9952, 0.0980) // e^{-i pi / 32}
                }
                if(target){ //control active 
                    if(v.v(q)){
                        if(fin){
                            val coeff = coefft(td)
                            val ph = new Complex((v.prop.real * coeff.real) - (v.prop.imag * coeff.imag), (v.prop.real * coeff.imag) + (v.prop.imag * coeff.real))
                            logger.debug(s"(Last) Done after CRZ: $world, with ${QVec(ph , v.v)}")
                            terminate(QVec(ph, v.v)) + step(()) + step(()) + step(())
                        }else{
                            val coeff = coefft(-1 * td)
                            val ph = new Complex((v.prop.real * coeff.real) - (v.prop.imag * coeff.imag), (v.prop.real * coeff.imag) + (v.prop.imag * coeff.real))
                            logger.debug(s"Applied CRZ: $world, with ${QVec(ph, v.v)}")
                            commit((QVec(ph, v.v), Circuit(gates.tail), world)) + step(()) + step(()) + step(())
                        }
                    }else{
                        if(fin){
                            //val coeff = coefft(td)
                            //val ph = new Complex((v.prop.real * coeff.real) - (v.prop.imag * coeff.imag), (v.prop.real * coeff.imag) + (v.prop.imag * coeff.real))
                            logger.debug(s"Done after CRZ: $world, with: ${v}")
                            terminate(v) + step(()) + step(()) + step(())
                        }else{ 
                            //val coeff = coefft(-1 * td)
                            //val ph = new Complex((v.prop.real * coeff.real) - (v.prop.imag * coeff.imag), (v.prop.real * coeff.imag) + (v.prop.imag * coeff.real))
                            logger.info(s"Applied RZ: $world, with: ${QVec(v.prop, v.v)}")
                            commit(v, Circuit(gates.tail), world) + step(()) + step(()) + step(())
                        }
                    }
                }else{
                    if(fin){
                       logger.debug(s"Done After CRZ: $world, with: ${v}")
                       terminate(v) + step(()) // just drop the gate and move on 
                    }else{
                       logger.debug(s"Applied CRZ: $world, with: ${v}")
                       commit(v, Circuit(gates.tail), world) + step(()) // just drop the gate and move on 
                    }
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
            case _ => logger.info("Something Went Wrong, I Don't Recognize The Gate")
        }
    

    def scaleAndSample() = {
        println("SCALING........")
        // terminate.volatileValue
        logger.debug(terminate.logSoup)
        println(terminate.volatileValue)
        val x = terminate.logSoup.split("Molecules:")(1)
        val each = x.split("terminate/P\\(QVec\\(")
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
        println("pps+----")
        scaled.foreach(println)
        val r = new BasicSampler(scaled.toList).sample()
        println("------------- RESULT -----------")
        println(pps)
        println(r)
    }



    //Basic DJ
    //commit((QVec(Complex(1d, 0), Vector(false, false)), Circuit(List(X(1), H(0), H(1), CX(0,1), CX(0,1), H(0))), "World 0")) + step(())

    // Example Simon Circuit 
    // val fourbitsimon = Circuit(List(H(0), H(1), CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), H(0), H(1)))
    // val fakeSimonSampler = new FakeSimonSampler(fourbitsimon)
    // val middleValue = fakeSimonSampler.sample()
    // for(i <- 0 to concurrentInstances - 1){
    //     commit((QVec(1d, Vector(false, false, false, false)), fourbitsimon, "World 0")) + step(()) + step(())
    // }   

    // Shor n=15
    // val _15bitshor = QVec(1d, Vector(false, false, false ,false ,false, false, false, false)) 
    // val n15shor = Circuit(List(H(0), H(1), H(2), H(3), X(0), X(1), X(2), X(3), 
    // ModGate(2, 15), 
    // H(0), CRotate(0, -2, 1), Swap(0, 1), CRotate(1, -4, 2), Swap(1, 2),
    // H(1), CRotate(2, -8, 3), Swap(2, 3), CRotate(1, -2, 2), Swap(1, 2), CRotate(2, -4, 3),
    // Swap(2, 3), H(2), CRotate(2, -2, 3), Swap(2, 3), H(3)
    // ))
    // for(i <- 0 to concurrentInstances - 1){
    //     commit((_15bitshor, n15shor, "World 0")) + step(())
    // }

    val _g0000 = QVec(1d, Vector(false, false, false, false))
    val grover0000 = Circuit(List(
        H(0), H(1), H(2), H(3),
        X(0), X(1), X(2), X(3),
        CRotate(0, 2, 3), CX(0, 1), CRotate(1, -2, 3), CX(0, 1), CRotate(1, 2, 3), CX(1, 2), CRotate(2, -2, 3), CX(0, 2), CRotate(2, 2, 3), CX(1, 2), CRotate(2, -2, 3), CX(0, 2), CRotate(2, 2, 3),
        X(1), X(0), X(2), X(3), H(0), H(1), H(2), H(3), //r2
        X(1), X(0), X(2), X(3),
        CRotate(0, 2, 3), CX(0, 1), CRotate(1, -2, 3), CX(0, 1), CRotate(1, 2, 3), CX(1,2), CRotate(2, -2, 3), CX(0, 2), CRotate(2, 2, 3), CX(1, 2), CRotate(2, -2, 3), CX(0, 2), CRotate(2, 2, 3),
        X(1), X(0), X(2), X(3), H(0), H(1), H(2), H(3)))

    commit((_g0000, grover0000, "World 0")) + step() 

    for(i <- 0 to concurrentInstances - 1){
        commit((_g0000, grover0000, "World 0")) + step() 
    }

}

