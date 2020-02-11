case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    for {
//      _ <- State.sequence(inputs.map(i => State.modify(moveNext(i))))
//      m <- State.get
//    } yield (m.candies, m.coins)
    val transitions = inputs.map(i => State.modify(moveNext(i)));
    State.sequence(transitions).flatMap(_ => State.get).map(m => (m.candies, m.coins))
  }
  
  private def moveNext(i: Input): Machine => Machine = m => {
    (i, m) match {
      case (_, Machine(_, 0, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Turn, Machine(true, _, _)) => m
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
    }
  }
}