package dcp.dcp19

/*
 *  This problem was asked by Facebook.
    
    A builder is looking to build a row of N houses that can be of K different colors. He has a goal of minimizing cost while ensuring that no two neighboring houses are of the same color.
    
    Given an N by K matrix where the nth row and kth column represents the cost to build the nth house with kth color, return the minimum cost which achieves this goal.
 */

object Solution {
  trait HasCost { val cost: Int }
  type Color = Int
  type Cost = Int
  case class ColorAndCost(color: Color, cost: Cost) extends HasCost {
    def merge(other: ColorAndCost): ColorAndCost = {
      ColorAndCost(color, cost + other.cost)
    }
  }
  type CostColumn = List[ColorAndCost]
  type CostMatrix = List[CostColumn]
  
  def runSample1(): Unit = {
    val mx = List(
        List(100, 10000),
        List(10, 1000),
        List(100, 50),
        List(1, 10)).map(column => mapWithIndex(column)((cost, ix) => ColorAndCost(ix, cost)))
        
    val result = solve(mx)
    println(result)
  }
  
  def runSample2(): Unit = {
    val mx = List(
        List(100, 10000, 1),
        List(10, 1000, 1),
        List(100, 50, 1),
        List(1, 10, 1)).map(column => mapWithIndex(column)((cost, ix) => ColorAndCost(ix, cost)))
        
    val result = solve(mx)
    println(result)
  }
  
  def solve(cm: CostMatrix): Int = {
    val solutions = subSolve(cm)
    solutions.map(s => s.cost).min
  }
  
  def subSolve(cm: CostMatrix): List[ColorAndCost] = cm match {
    case Nil => Nil
    case column::Nil => twoMinsByCost(column)
    case column::submatrix => {
      val columnMins = twoMinsByCost(column)
      val restMins = subSolve(submatrix)
      // Find possible solutions by joining current column's solutions to the rest of the matrix' solutions
      // enforcing the condition that two neighbor columns cannot have the same color
      val solutions = join(columnMins, restMins)((s1, s2) => s1.color != s2.color)
        .map(x => x._1 merge x._2)
      
      // get two most cost-effective solutions
      twoMinsByCost(solutions)
    }
  }
  
  def join[A](left: Seq[A], right: Seq[A])(p: (A,A) => Boolean): List[(A,A)] = {
    if (left.isEmpty) scala.List.empty[(A,A)]
    else {
      // inner-loops implementation of join
      
      val curLeft = left.head
      val curSeq = right.map(r => (curLeft, r)).filter(x => p(x._1, x._2)).toList
      curSeq ++ join(left.tail, right)(p)
    }
  }
    
  def twoMinsByCost[A <: HasCost](as: List[A]): List[A] =
    twoMins(as)((a1, a2) => a1.cost < a2.cost)
  
  def twoMins[A](as: List[A])(lt: (A, A) => Boolean): List[A] =
    as.foldLeft(Nil:List[A])((acc, a) => {
      acc match {
        // insert into sorted list by ascending
        case min1::min2::_ => {
          if (lt(a, min1)) List(a, min1) 
          else if (lt(a, min2)) List(min1, a)
          else acc
        }
        case min::_ => {
          if (lt(a, min)) List(a, min)
          else List(min, a)
        }
        case Nil => List(a)
      }
    })
    
  def mapWithIndex[A,B](as: List[A])(f: (A, Int) => B): List[B] = {
    as.zipWithIndex.map(x => f(x._1, x._2))
  }
}