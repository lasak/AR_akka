package pl.edu.agh.ar.salesman

import akka.actor._
import akka.actor.Actor._
import scala.util.Random
import pl.edu.agh.ar.salesman.Solution._
import scala.annotation.tailrec
import scala.util.continuations._

case class Chunk(val solution: Solution, val towns: List[String]) {
  def remains = towns.size
}

case object GetChunk
case class HereIsChunk(val c: Chunk)
case class NoMoreChunks()
case class BoundExchange(val bound: Bound)
case class BoundResponse(val bound: Bound)
case class Finished(val bound: Bound)


trait Node extends Actor with Stash {
  var bound: Bound = None
  val problem: Problem
  
  val id = Random.nextInt(1000)
  
  def log(s: String) {
    val clazz = getClass().getSimpleName()
    val msg = s"${clazz}[${id}]: " + s
    println(msg)
  }
  
  def updateBound(b: Bound): Unit @cps[Unit] = {
    bound = takeBest(bound, b)
  }

  def exchangeBound(a: ActorRef): Unit @cps[Unit] =
    shift { k: (Unit => Unit) =>
      a ! BoundExchange(bound)
      context become waitForBound(k)
    }
  
  private def waitForBound(k: Unit => Unit): Receive = {
    case BoundResponse(b) => 
      bound = b
      unstashAll()
      context.unbecome()
      k()
    case _ => stash()
  }

  def worthTrying(s: Solution) = bound match {
    case Some(best) if s.cost >= best.cost => false
    case _ => true
  }

  def getNewChunk(): Option[Chunk] @cps[Unit]
  def finish()
}


trait Broker extends Node {

    /*
   * TODO zmieniæ ¿eby budowaæ poddrzewo na jednym poziomie
   */
  var chunks: List[Chunk] = Nil
  var exchangeCount = 0
  var finishedChildren = 0
  val children: Int
  val desiredChunks: Int = 100

  def requiredExpansionDepth(size: Int, target: Int) = {
    var s = 1
    var level = 0
    while (s < target) {
      level += 1
      s *= (size + level)
    }
    level
  }


  def splitChunk(chunk: Chunk, n: Int): List[Chunk] = {
    val depth = requiredExpansionDepth(2, n)

    chunk.towns.splitAt(depth) match {
      case (_, Nil) => List(chunk)
      case (towns, tail) =>
        val bs = branches(problem, towns, chunk.solution).toList
        bs map { b => Chunk(b, tail) }
    }
  }

  def takeNextChunk(): Option[Chunk] @cps[Unit] = {
    chunks = chunks.dropWhile(c => !worthTrying(c.solution))
    chunks match {
      case t :: ts =>
//        log(s"${ts.size} remains")
        chunks = ts
        Some(t)
      case Nil => getNewChunk() match {
        case Some(chunk) =>
          chunks = splitChunk(chunk, desiredChunks)
          takeNextChunk()
        case None => None
      }
    }
  }

  def receive = {
    case BoundExchange(b) => reset {
      val guy = sender
      updateBound(b)
      exchangeCount += 1
      guy ! BoundResponse(bound)
    }

    case GetChunk => reset {
      val guy = sender
      takeNextChunk() match {
        case Some(s) => guy ! HereIsChunk(s)
        case None => guy ! NoMoreChunks
      }
    }

    case Finished(b) =>
      finishedChildren += 1
      if (finishedChildren == children)
        finish()
  }

}

trait NodeWithParent extends Node {

  val parent: ActorRef

  def getNewChunk(): Option[Chunk] @cps[Unit] = {
    exchangeBound(parent)
    shift { k: (Option[Chunk] => Unit) => 
      parent ! GetChunk
      context become waitForChunk(k)
    }
  }

  private def waitForChunk(k: Option[Chunk] => Unit): Receive = {
    case HereIsChunk(chunk) =>
      unstashAll()
      context.unbecome()
      k(Some(chunk))
    case NoMoreChunks =>
      unstashAll()
      context.unbecome()
      k(None)
    case _ => stash()
  }

  def finish() {
    log("Done.")
    parent ! Finished(bound)
  }
}

class Master(val problem: Problem, val children: Int, a: ActorRef) extends Broker {

  val rootChunk = Chunk(EmptySolution, problem.towns)
  chunks = splitChunk(rootChunk, desiredChunks)

  override def getNewChunk() = None

  override def finish() {
    a ! Finished(bound)
  }
}

class Internal(
  val problem: Problem,
  val parent: ActorRef,
  val children: Int) extends Broker with NodeWithParent {

  val exchangeFreqency = 5

  override def updateBound(b: Bound): Unit @cps[Unit] = {
    super.updateBound(b)
    if (exchangeCount % exchangeFreqency == 0) {
      exchangeBound(parent)
    }
  }
}

class Worker(val problem: Problem, val parent: ActorRef) extends NodeWithParent {

  var leafsVisited = 0
  val exchangeFrequency = 10000

  def shouldExchange = (leafsVisited + 1) % exchangeFrequency == 0

  def visitLeaf(s: Solution): Unit @cps[Unit] = {
    bound = Some(s)
    endOfBranch()
  }

  def endOfBranch(): Unit @cps[Unit] = {
    leafsVisited += 1
    if (shouldExchange) {
      exchangeBound(parent)
    }
  }
  
  def receive = {
    case _ => act()
  }

  final def act() {
    reset {
      getNewChunk() match {
        case Some(chunk) =>
          search(chunk)
          act()
        case None =>
          finish()
      }
    }
  }

  def search(chunk: Chunk): Unit @cps[Unit] = {
    
    import CpsConversions.cpsIterable
    
    def visit(towns: List[String], s: Solution): Unit @cps[Unit] = {
      if (worthTrying(s)) {
        towns match {
          case Nil => visitLeaf(s)
          case t :: ts =>
            branches(problem, t, s).cps foreach(visit(ts, _))
        }
      } else {
        endOfBranch()
      }
    }
    visit(chunk.towns, chunk.solution)
  }
}
