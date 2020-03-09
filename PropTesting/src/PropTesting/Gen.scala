package PropTesting

case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))
    
  def generate(rng: RNG): (A, RNG) =
    sample.run(rng)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Random.inRange(start, stopExclusive))
    
  def randStr(len: Int, alphabet: String): Gen[String] = {
    val charGen = choose(0, alphabet.length).map(ix => alphabet(ix))
    listOfN(len, charGen).map(charlist => charlist.mkString(""))
  }
    
  def pairInRange(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(Random.map2(Random.inRange(start, stopExclusive), Random.inRange(start, stopExclusive))((_, _)))
    
  def option[A](g: Gen[A]): Gen[Option[A]] = {
    val rand = boolean.sample.map2(g.sample)((success, a) => {
      if (success) Some(a)
      else None: Option[A]
    })
    Gen(rand)
  }
    
  def unit[A](a: => A): Gen[A] =
    Gen(Random.unit(a))
    
  def boolean: Gen[Boolean] =
    Gen(Random.inRange(0, 2).map(i => i == 0))
    
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(Random.sequence(List.fill(n)(g.sample)))
}