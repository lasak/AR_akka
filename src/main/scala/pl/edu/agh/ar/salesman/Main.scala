package pl.edu.agh.ar.salesman

import scala.util.Random
import pl.edu.agh.ar.salesman.Solution._
import akka.actor._
import scala.util.continuations._
import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config

case object Run

case class Start(
  master: ActorRef,
  problem: Problem,
  workers: Int)
  
case class Stop()

object Cfg {
  
  val conf = ConfigFactory.load
  
  def makeConfig(host: String, port: Int): Config = {
    val s = s"akka.remote.netty.tcp.port     = ${port}\n" +
          s"""akka.remote.netty.tcp.hostname = "${host}""""
    ConfigFactory parseString(s) withFallback(conf)
  }
}

class NodeManager extends Actor {
  
  println("Node manager creation")
  
  override def preStart() {
    println("Node manager is starting")
  }
  
  override def receive = {
    case s : Start =>
      println(s"Got call to arms: " + s)
      createActorTree(s)
      
    case _ : Stop =>
      println("Done!")
      context.system.shutdown()
      
    case x => println("Dafuq :/ " + x)
  }

  def createActorTree(s: Start) {
    val props = Props(classOf[Internal], s.problem, s.master, s.workers)
    val localBroker = context.actorOf(props, "LocalBroker")

    for (i <- 1 to s.workers) {
      val props = Props(classOf[Worker], s.problem, localBroker)
      val worker = context.actorOf(props, s"Worker${i}")
      worker ! Run
    }
  }
}

object WorkerMain extends App {
  
//  val as = Array("127.0.0.1", "6666")
  
  println("Worker starting up!")
  
  val Array(host, portStr) = args
  val port = portStr toInt
  
  val conf = Cfg.makeConfig(host, port) 
  val system = ActorSystem("workers", conf)
  println("Got actor system")
  
  val manager = system.actorOf(Props[NodeManager], "Manager")
  println("Manager created in main")
}


case class Remote(val host: String, val port: Int)


object Main extends App {

  class Root extends Actor {
    
    val before = System.currentTimeMillis()

    def receive = {
      case Finished(bound) =>
        val after = System.currentTimeMillis()
        val dt = (after - before) / 1000.0
        println(s"{ 'total': ${dt} }")
//        showSolution(bound)
        stopRemotes()
        context.system.shutdown()
    }
  }
  
  val MAX_DISTANCE = 50
  
  
  Random.setSeed(1)
  def makeDistance = Random.nextInt(MAX_DISTANCE)

//  val as = Array("127.0.0.1", "16", "4", "127.0.0.1:6666")
  val Array(host, nStr, workersStr, urls @ _*) = args

  val n = nStr toInt
  val towns = List.tabulate(n) (_.toString())
  val roads = for (town1  <- towns; town2 <- towns if town1 != town2) yield 
    (town1, town2, makeDistance)
    
  val problem = Problem(towns, roads)

  val workers = workersStr toInt
  val port = 2999
  
  val remotes = parseRemotes(urls)

  val conf = Cfg.makeConfig(host, port)
  val system = ActorSystem("main", conf)

  val root = system.actorOf(Props[Root], "Root")
  val master = system.actorOf(Props(classOf[Master], problem, 1, root), "Master")

  startRemotes(workers)
  
  def parseRemotes(args: Seq[String]) = args map { s =>
    val Array(host, port) = s split ":"
    Remote(host, port toInt)
  }
  
  def startWorker(r: Remote, workers: Int) {
    val manager = getRemoteSystem(r)
    manager ! Start(master, problem, workers)
  }
  
  def stopWorker(r: Remote) = getRemoteSystem(r) ! Stop()

  def startRemotes(workers: Int) = remotes foreach (startWorker(_, workers)) 
  
  def stopRemotes() = remotes foreach (stopWorker(_))
  
  def getRemoteSystem(r: Remote) = {
    val systemName = "workers"
    val actor = "Manager"
    val url = s"akka.tcp://${systemName}@${r.host}:${r.port}/user/${actor}"
    
    system.actorSelection(url)
  }

  def showSolution(os: Option[Solution]) {
    os match {
      case Some(s) =>
        println(s"Solution:\n${Solution}")
        println(s"Total cost: ${s.cost}")
      case None => println("No solution!")
    }
  }

}