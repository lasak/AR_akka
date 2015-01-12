package pl.edu.agh.ar.salesman

case class Problem(
    towns: List[String],
    roads: List[(String, String, Int)]
  )

case class Solution(
  journey: List[String],
  cost: Int) {

  def this() = this(List(), 0)

  def addTown(town: String, costDiff: Int) =
    Solution(
      journey = town :: journey,
      cost = cost + costDiff)
}

object EmptySolution extends Solution

object Solution {
  
  type Bound = Option[Solution]
  
  def findCost(r:List[(String, String, Int)], town1:String, town2:String) : Int = { 
    r match {
      case (town1, town2, c) :: ts => c
      case _ :: ts => findCost(ts, town1, town2)
      case Nil => Int.MaxValue
    }
  }
  
  
  def branches(p: Problem, town: String, s: Solution): Iterable[Solution] = {
    val solutions = List.empty
    for (town <- p.towns) {
      if (!s.journey.contains(town)) {
        val lastTown : String = s.journey match {
          case t :: ts => t
          case Nil => ""
        }
    
        val cost = 
          if (lastTown == "") 0
          else findCost(p.roads, lastTown, town)
    
        solutions ++ Seq(s.addTown(town,cost))
      }
    }
    solutions

  }
  
  def branches(p: Problem, towns: List[String], s: Solution): Iterable[Solution] = towns match {
    case t :: ts => branches(p, t, s).flatMap(sol => branches(p, ts, sol))
    case Nil => List(s)
  }
    
  def takeBest[T](o1: Bound, o2: Bound): Bound = {
    (o1, o2) match {
      case (Some(t1), Some(t2)) => Some(better(t1, t2))
      case (Some(t1), None) => o1
      case (None, Some(t2)) => o2
      case (None, None) => None
    }
  }
  
  def better(s1: Solution, s2: Solution) = if (s1.cost < s2.cost) s1 else s2
}