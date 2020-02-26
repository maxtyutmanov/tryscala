import core.Par
import java.util.concurrent.Executors


object Program {
  def main(args: Array[String]): Unit = {
    testChoiceN
  }
  
  def testChoiceN: Unit = {
    val es = Executors.newWorkStealingPool();
    val l = List(core.Par.unit(1), core.Par.unit(2), core.Par.unit(3))
    val n = core.Par.fork(core.Par.unit(2))
    
    val chosen = core.Par.choiceN(n)(l)
    println(chosen(es).get)
  }
  
  def testNonBlocking: Unit = {
    val es = Executors.newWorkStealingPool();
    val (a, b) = (corenb.Par.unit(1), corenb.Par.unit(2))
    val resultPar = corenb.Par.map2[Int, Int, Int](a, b)((_, _) => throw new Exception("BAD"))
    
    try {
      val result = corenb.Par.run(es)(resultPar)
      println(result)
    }
    catch {
      case e: Exception => println("BAD")
    }
  }
  
  def testMap3: Unit = {
    val es = Executors.newWorkStealingPool();
    
    val (a, b, c) = (Par.unit(1), Par.unit(2), Par.unit(3))
    
    val resultPar = Par.map3(a, b, c)(_ + _ + _)
    val result = resultPar(es).get()
    
    println(result);
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