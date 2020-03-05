package PropTesting

case object Random {
  def unit[A](a: A): Rand[A] = State.unit[RNG,A](a)
    
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = s.map(f)
  
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = f.flatMap(g)
  
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ra.map2(rb)(f)
    
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    ra.map2(rb)((a, b) => (a, b))
    
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = State.sequence(fs)
  
  def nextDouble(rng: RNG): (Double, RNG) = 
    map(State(nonNegativeInt))(i => i / (Int.MaxValue + 1.0)).run(rng)
    
  val int: Rand[Int] = State(_.nextInt)
  val double: Rand[Double] = State(nextDouble)
  val nnint: Rand[Int] = State(nonNegativeInt)
  
  def inRange(minInclusive: Int, maxExclusive: Int): Rand[Int] =
    nonNegativeLessThan(maxExclusive - minInclusive).map(x => minInclusive + x)
  
  def intDouble(rng: RNG): ((Int,Double), RNG) = 
    both(int, double).run(rng)
  
  def doubleInt(rng: RNG): ((Double,Int), RNG) = 
    both(double, int).run(rng)
  
  def double3(rng: RNG): ((Double,Double,Double), RNG) = 
    map(sequence(List[Rand[Double]](double, double, double)))(l => (l(0), l(1), l(2))).run(rng)
    
  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
  
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, nextRNG) = rng.nextInt
    if (x == Int.MinValue) (math.abs(x + 1), nextRNG)
    else (math.abs(x), nextRNG)
  }
  
  def nonNegativeEven: Rand[Int] = 
    map(State(nonNegativeInt))(i => i - i % 2)
    
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(State(nonNegativeInt))(i => {
      val mod = i % n
      
      if (i + (n - 1) >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })
}