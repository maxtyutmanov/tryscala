import core.Par
import java.util.concurrent.Executors


object Program {
  def main(args: Array[String]): Unit = {
    val list = List(1, -2, 44, 15, -10)
    val parMin = Par.aggregate(list, Int.MaxValue)(math.min)(math.min);
    
    val es = Executors.newWorkStealingPool();
    println(parMin(es).get());
  }
}