package PropTesting

case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))
    
  def flatMap[B](f: A => Gen[B]): Gen[B] = 
    Gen(sample.flatMap(a => f(a).sample))
    
  def generate(rng: RNG): (A, RNG) =
    sample.run(rng)
}

object Gen {
  def join[A](gg: Gen[Gen[A]]): Gen[A] =
    gg.flatMap(ga => ga)
    
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)
    
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val scaledW1 = g1._2 / (g1._2 + g2._2)
    Gen(Random.double).flatMap(d => if (d < scaledW1) g1._1 else g2._1)
  }
    
  def map2[A,B,C](ga: Gen[A], gb: Gen[B])(f: (A, B) => C): Gen[C] = for {
    a <- ga
    b <- gb
  } yield f(a, b)
  
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Random.inRange(start, stopExclusive))
  
  def randStr(len: Gen[Int], alphabet: String): Gen[String] = {
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
    
  def listOfN[A](n: Gen[Int], g: Gen[A]): Gen[List[A]] =
    n.flatMap(nval => listOfN(nval, g))
}