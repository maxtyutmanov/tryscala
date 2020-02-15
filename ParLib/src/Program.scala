import core.Par
import java.util.concurrent.Executors


object Program {
  def main(args: Array[String]): Unit = {
    testWordcount
  }
  
  def testWordcount: Unit = {
    val es = Executors.newWorkStealingPool();
    
    val pars = List("a b cc", "def\tq", "ddd d ddd ddddd")
    val count = wordCount(pars)(es).get()
    println(count)
  }
  
  def testAggregate: Unit = {
    val list = List(1, -2, 44, 15, -10)
    val parMin = Par.aggregate(list, Int.MaxValue)(math.min)(math.min);
    
    val es = Executors.newWorkStealingPool();
    println(parMin(es).get());
  }
  
  def wordCount(ps: List[String]): Par[Int] = {
    val countsByParagraph = Par.parMap(ps)(wordCount)
    Par.map(countsByParagraph)(_.sum)
  }
  
  def wordCount(s: String): Int = s.count(isDelimiter) + 1
  
  def isDelimiter(c: Char): Boolean = Character.isWhitespace(c)
}