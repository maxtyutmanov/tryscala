package PropTesting

case class Gen[A](sample: State[RNG,A]) {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Random.inRange(start, stopExclusive))
}