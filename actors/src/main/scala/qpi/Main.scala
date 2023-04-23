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

trait Gate 
case object X extends Gate 
case class H(ctrl: Int) extends Gate 
case class CX(ctrl: Int, target: Int) extends Gate
case object NullGate extends Gate
case object Measure extends Gate

case object InvalidCircuitException extends NoStackTrace

object BuildCircuit{

  val hScale : Double = 1.0 / Math.sqrt(2.0)
  val distributionConstant = 5;
  val decider = new Random()

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
      res: List[Result]
  ): Behavior[Message]  = 
    Behaviors.setup{ (context) => { 
      Behaviors.receiveMessage{(message)  => {
        message match {
          case pr @ PartialResult(_, _) => 
            if(res.length + 1 == children.length ){
              println("Received results from all the children")
              parent match{
                case None =>
                  println("------------REPORTING-------------")
                  println(res :+ pr)
                case Some(prtn) => 
                  val updatedRes = PartialResult((res :+ pr).flatMap{
                    case prr @ PartialResult(_, _) => prr.result //TODO: need to roll the dice again
                    case sr @ Stop(_, _) => sr.result
                    case _ => throw InvalidCircuitException
                  }, context.self)
                  prtn ! updatedRes
              }

            }else{
              println("Waiting for more results")
            }
            buildQActor(g, parent, children, res :+ pr)
          case pr @ Stop(_, _) => //TODO: this actually needs to be discarded
            if(res.length + 1 == children.length ){
              println("Received results from all the children")
              parent match{
                case None =>
                  println("------------REPORTING-------------")
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
            buildQActor(g, parent, children, res :+ pr)
          case a : QVec => evalV(a, g, parent, context)
          case a : Circuit => processCircuit(a, parent, children, context)
          case Ok => 
              println("Received OK")
              Behaviors.same 
          case _ => throw InvalidCircuitException
        }
        }
      }
    }
  }


  def evalV(
    a: QVec, 
    g: Gate,
    parent: Option[ActorRef[Message]],
    context: ActorContext[Message]
  ) : Behavior[Message] = 
    g match { 
      case Measure => 
        println("Measurement: " +  a.prop + " " + a.v.toString)
        if(decider.nextInt(10) < distributionConstant){
          parent.get ! PartialResult(List(a), context.self)
        }else{
          parent.get ! Stop(List(a), context.self)
        }
        Behaviors.same
      case H(t) => 
        println("Hadamard") 
        val cld = context.children.toList 
        val x1 = QVec(hScale * a.prop, a.v)
        val x2 =  QVec(hScale * a.prop, a.v.updated(t, !a.v(t)))
        if(cld.length != 2){
          throw InvalidCircuitException
        }else{
          cld(0).unsafeUpcast[Message] ! x1
          cld(1).unsafeUpcast[Message] ! x2
        }
        Behaviors.same
      case X  =>       
        println("Pauli")
        context.children.foreach(c => c.unsafeUpcast[Message] ! (QVec(a.prop, a.v)))
        Behaviors.same
      case _ @ CX(c, t) => 
        println("Controlled-X")
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
        println("OTHER " + g)
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
        println("Building Measurement Gate")
        a.replyTo ! Ok
        buildQActor(Measure, parent, children, List.empty)  
      case x :: xs => 
        x match{
          case H(_)=>
            println("Building " + x + " Gate")
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
            println("Building a " + x + " Gate")
            val next = context.spawn(BuildCircuit(Some(context.self)), "next")
            next.ask((_ : ActorRef[Message])  => Circuit(xs, context.self)).onComplete {
              case Success(_) => a.replyTo ! Ok
              case Failure(ex) => println(ex)
            }
            buildQActor(x, parent, List(next), List.empty)
          case CX(_, _) => 
            println("Building a " + x + " Gate")
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


