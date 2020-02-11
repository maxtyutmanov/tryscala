object Program {
  def main(args: Array[String]): Unit = {
    val m: Machine = Machine(true, 10, 10);
    val m2 = Machine.simulateMachine(List(Coin, Turn, Coin, Turn)).run(m)
    println(m2);
  }
}