package pl.edu.agh.ar.salesman

import scala.util.Random
import pl.edu.agh.ar.salesman.Solution._


object SequentialAlgorithm {

  def worthTrying(bound: Bound, s: Solution) = bound match {
    case Some(best) if s.cost >= best.cost => false
    case _ => true
  }
  
  def search(p: Problem): Bound = {
    var bound: Bound = None

    def visit(towns: List[String], s: Solution) {
      if (worthTrying(bound, s)) {
        towns match {
          case Nil => bound = Some(s)
          case t :: ts => for (b <- branches(p, t, s)) visit(ts, b)
        }
      }
    }
    
    visit(p.towns, EmptySolution)
    bound
  }

  def searchFunctional(p: Problem): Option[Solution] = {

    def traverse(towns: List[String], s: Solution, bound: Bound): Bound = {
      towns match {
        case Nil => takeBest(Some(s), bound)
        case t :: ts =>
          if (worthTrying(bound, s))
            branches(p, t, s).foldLeft(bound) { (b, path) =>
              val nextBound = traverse(ts, path, b)
              takeBest(b, nextBound)
            }
          else bound
      }
    }
    return traverse(p.towns, EmptySolution, None)
  }
  
}