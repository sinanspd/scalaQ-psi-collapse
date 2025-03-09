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

//This is just a playground file to test different CHAM features
object Trial extends App {

    val tp = BlockingPool(8) 

    val mymole = m[Int]
    val step = m[Unit]
    var med = false
    site(tp) (
        go { case mymole(a) + mymole(b) ⇒ { 
                if(a + b > 10){
                }else{
                    println(s"Mol 1: $a, Mol 2: $b")
                    //mymole(a + b) + mymole(1)
                    f(a, b)
                    // println(mymole.logSoup)
                    //tp.shutdownNow()
                } 
           }
        },
        go{ 
            case mymole(a) + step (()) ⇒ { 
                if(a + 1 > 10){}else{
                    println(s"Mol 1: $a, Mol 2: $b")
                    f(a, 1)
                }
            }
            
        }
    )

    def f(a: Int, b: Int){
        mymole(a + b) + step(()) //+ mymole(1)
    }

    mymole(1) + step(())  // + mymole(1)
}