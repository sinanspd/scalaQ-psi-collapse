package example

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.{Success,Failure}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NoStackTrace
import scala.util.Random
import org.slf4j.LoggerFactory

trait Gate 
case object X extends Gate 
case class H(ctrl: Int) extends Gate 
case class CX(ctrl: Int, target: Int) extends Gate
case object NullGate extends Gate
case object Measure extends Gate

case object InvalidCircuitException extends NoStackTrace

object BuildCircuit{

  val hScale : Double = 1.0 / Math.sqrt(2.0)
  val distributionConstant = 11
  val decider = new Random()
  val logger = LoggerFactory.getLogger("example.BuildCircuit")
  val mode = true;

  implicit val timeout: Timeout = 3.seconds

  implicit val system: ActorSystem[BuildCircuit.Message] =
    ActorSystem(BuildCircuit(None), "qsys")

  trait Message 
  final case class Circuit(remainingGates: List[Gate], replyTo: ActorRef[Message]) extends Message
  final case class QVec(prop: Double, v: Vector[Boolean]) extends Message 
  final case object Ok extends Message

  trait Result extends Message
  final case class PartialResult(result: List[QVec], from: ActorRef[Message]) extends Result
  final case class Stop(result: List[QVec], from: ActorRef[Message]) extends Result

  def apply(parent: Option[ActorRef[Message]]): Behavior[Message] = 
    buildQActor(NullGate, parent, List.empty, List.empty)
  
  def buildQActor(
      g: Gate, 
      parent: Option[ActorRef[Message]], 
      children: List[ActorRef[Message]],
      res: List[QVec],
      reported: Boolean = false
  ): Behavior[Message]  = 
    Behaviors.setup{ (context) => { 
      Behaviors.receiveMessage{(message) => {
        message match {
          case pr @ PartialResult(_, _) => 
            if(mode){
              val allResultsSoFar : List[QVec] = res ++ pr.result
              val probsByResult = allResultsSoFar.groupBy(_.v).view.mapValues(_.foldLeft(0.0)((a, b) => a + b.prop)).toMap
              val satisfactoryResults = probsByResult.filter(t => t._2 > 0.45).toList
              val updatedResults : List[QVec] = 
                  probsByResult.foldLeft(List.empty[QVec])((r : List[QVec], t : Tuple2[Vector[Boolean], Double]) => r :+ QVec(t._2, t._1))
              if(satisfactoryResults.length > 0){
                logger.debug("Satisfactory Results: " + satisfactoryResults.toString)
                val selectedResult = decider.nextInt(satisfactoryResults.length)
                parent match{
                  case None => 
                    if(!reported){
                      logger.info("------------- REPORTING ------------- \n " + satisfactoryResults(selectedResult)._1)
                      buildQActor(g, parent, children, updatedResults, true)
                    }else{
                      buildQActor(g, parent, children, updatedResults, reported)
                    }
                  case Some(p) => 
                    val res = satisfactoryResults(selectedResult)
                    p ! Stop(List(QVec(res._2, res._1)), context.self)
                    buildQActor(g, parent, children, updatedResults, reported)
                }
              }else{
                parent match{
                  case None => ()
                  case Some(prtn) => 
                    val changedVecs = pr.result.map(_.v)
                    val changedResults = updatedResults.filter(qv => changedVecs.contains(qv.v)) 
                    prtn ! PartialResult(changedResults, context.self)
                }
                buildQActor(g, parent, children, updatedResults, reported)
              }
            }else{
              if(res.length + 1 == children.length) {
                println("Received results from all the children")
                parent match{
                  case None =>
                    println("------------REPORTING LOCAL -------------")
                    println(res :+ pr)
                  case Some(prtn) => 
                    val updatedRes = PartialResult((res :+ pr).flatMap {
                      case prr @ PartialResult(_, _) => prr.result //TODO: need to roll the dice again
                      case sr @ Stop(_, _) => sr.result
                      case _ => throw InvalidCircuitException
                    }, context.self)
                    prtn ! updatedRes
                }

              }else{
                println("Waiting for more results")
              }
              buildQActor(g, parent, children, res ++ pr.result)
            }
          case pr @ Stop(_, _) => //TODO: this actually needs to be discarded
            if(mode){
                parent match{
                  case None => 
                    if(!reported){
                      logger.info("\n ------------- REPORTING ------------- \n " + pr.result.head.v)
                      buildQActor(g, parent, children, res, true)
                    }else{
                      Behaviors.same
                    }
                  case Some(p) => 
                    p ! pr
                    Behaviors.same
                }
            }else{
              if(res.length + 1 == children.length) {
                println("Received results from all the children")
                parent match{
                  case None =>
                    println("------------REPORTING LOCAL -------------")
                    println(res)
                  case Some(prtn) => 
                    val updatedRes = PartialResult((res :+ pr).flatMap{
                      case prr @ PartialResult(_, _) => prr.result
                      case sr @ Stop(_, _) => sr.result
                      case _ => throw InvalidCircuitException
                    }, context.self)
                    prtn ! updatedRes
                }
              }else{
                println("Waiting for more results")
              }
              buildQActor(g, parent, children, res ++ pr.result)
            }
          case a : QVec => evalV(a, g, parent, context)
          case a : Circuit => processCircuit(a, parent, children, context)
          case Ok => 
              logger.debug("Received OK")
              Behaviors.same 
          case _ => throw InvalidCircuitException
        }
      }}
    }}

  def evalV(
    a: QVec, 
    g: Gate,
    parent: Option[ActorRef[Message]],
    context: ActorContext[Message]
  ) : Behavior[Message] = 
    g match { 
      case Measure => 
        logger.debug("Measurement: " +  a.prop + " " + a.v.toString)
        if(mode){
          parent.get ! PartialResult(List(a), context.self)
        }else{
          if(decider.nextInt(10) < distributionConstant) {
            parent.get ! PartialResult(List(a), context.self)
          } else {
            parent.get ! Stop(List(a), context.self)
          }
        }
        Behaviors.same
      case H(t) => 
        val cld = context.children.toList 
        val sign = if (a.v(t)) -1 else 1
        val x1 = QVec(sign * hScale * a.prop, a.v)
        val x2 =  QVec(hScale * a.prop, a.v.updated(t, !a.v(t)))
        logger.debug("Evaluting Hadamard: " + x1 + " and " + x2 + " original: " + a.v + " t is: " + t)
        if(cld.length != 2) {
          throw InvalidCircuitException
        } else {
          cld(0).unsafeUpcast[Message] ! x1
          cld(1).unsafeUpcast[Message] ! x2
        }
        Behaviors.same
      case X  =>       
        logger.debug("Evaluating Pauli-X")
        context.children.foreach(c => c.unsafeUpcast[Message] ! (QVec(a.prop, a.v)))
        Behaviors.same
      case _ @ CX(c, t) => 
        logger.debug("Evaluating Controlled-X")
        if(a.v.length >= c){
          if(a.v(c)){
            context.children.foreach(c => c.unsafeUpcast[Message] ! (QVec(a.prop, a.v.updated(t, !(a.v(t))))))
          }else{
            context.children.foreach(c => c.unsafeUpcast[Message] ! QVec(a.prop, a.v))
          }
        }else{
          throw InvalidCircuitException
        }
        Behaviors.same
      case _ => 
        logger.debug("Evaluating Other Gate: " + g)
        Behaviors.same
    }

  def processCircuit(
    a: Circuit, 
    parent: Option[ActorRef[Message]],
    children: List[ActorRef[Message]],
    context: ActorContext[Message]
  ) : Behavior[Message] = 
    a.remainingGates match{
      case Nil => 
        logger.debug("Building Measurement Gate")
        a.replyTo ! Ok
        buildQActor(Measure, parent, children, List.empty)  
      case x :: xs => 
        x match{
          case H(_)=>
            logger.debug("Building " + x + " Gate")
            val childLeft = context.spawn(BuildCircuit(Some(context.self)), "childLeft")
            val childRight = context.spawn(BuildCircuit(Some(context.self)),  "childRight")
            val f1 = childLeft.ask((_ : ActorRef[Message])  => Circuit(xs, context.self))
            val f2 = childRight.ask((_ : ActorRef[Message])  => Circuit(xs, context.self))
            Future.sequence(List(f1, f2)).onComplete {
              case Success(_) => a.replyTo ! Ok
              case Failure(ex) => println(ex)
            }
            buildQActor(x, parent, List(childLeft, childRight), List.empty)
          case X => 
            logger.debug("Building a " + x + " Gate")
            val next = context.spawn(BuildCircuit(Some(context.self)), "next")
            next.ask((_ : ActorRef[Message])  => Circuit(xs, context.self)).onComplete {
              case Success(_) => a.replyTo ! Ok
              case Failure(ex) => println(ex)
            }
            buildQActor(x, parent, List(next), List.empty)
          case CX(_, _) => 
            logger.debug("Building a " + x + " Gate")
            val next = context.spawn(BuildCircuit(Some(context.self)), "next")
            next.ask((_ : ActorRef[Message]) => Circuit(xs, context.self)).onComplete {
              case Success(_) => a.replyTo ! Ok
              case Failure(ex) => println(ex)
            }
            buildQActor(x, parent, List(next), List.empty)    
          case _ => throw InvalidCircuitException
        }
    }
  


  def main(args: Array[String]): Unit = {

    //Base Case Examples
    //system.ask(ref => Circuit(List(X, H(0), CX(0, 1)), ref))

    //Simon's Problem n=2 a=3
    system.ask(ref => Circuit(List(H(0), H(1), CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), H(0), H(1)), ref))

    system ! QVec(1d, Vector(false, false, false, false))
  }
}


