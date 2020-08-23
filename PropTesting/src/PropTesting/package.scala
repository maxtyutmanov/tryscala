import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit


package object PropTesting {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type Rand[+A] = State[RNG,A]
  type Par[A] = ExecutorService => Future[A]
  
  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = new SimpleRNG(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n, tag) => println(s"Falsified after $n successful tests:\r\n$msg")
      case Passed => println("Passed")
      case Proved => println("Proved")
    }
  }
  
  def forAll[A](g: SGen[A])(f: A => Boolean)(propTag: String): Prop =
    forAll(g.forSize)(f)(propTag)
  
  def forAll[A](g: Int => Gen[A])(f: A => Boolean)(propTag: String): Prop = Prop {
    (maxSize: MaxSize, numCases: TestCases, rng: RNG) => {
      // equal number of cases for each size (sucks)
      val casesPerSize = (numCases / maxSize) max 1
      
      // create properties for each input size, starting from zero
      val propsForEachSize: Stream[Prop] =
        Stream.from(0).take(maxSize + 1).map(i => forAll(g(i))(f)(propTag))
        
      // unify these properties into one, overriding the numCases for their invocations
      val unifiedProp = propsForEachSize.map(prop => {
        Prop((m, _, r) => {
          prop.run(m, casesPerSize, r)
        })
      }).reduce(_ && _)
      
      unifiedProp.run(maxSize, numCases, rng)
    }
  }
  
  def forAll[A](ga: Gen[A])(f: A => Boolean)(propTag: String): Prop = {
    val run = (maxSize: MaxSize, numCases: TestCases, rng: RNG) => {
      (randomStream(ga)(rng) zipWithIndex).take(numCases).map {
        case (a, i) => try {
          if (f(a)) Passed
          else Falsified(a.toString(), i, propTag)
        } catch {
          case (e: Exception) => Falsified(getMsg(a, e), i, propTag)
        }
      }.find(x => x.isFalsified).getOrElse(Passed)
    }
    
    Prop(run)
  }
  
  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean])(propTag: String): Prop =
    forAll(Gen.TP ** g) { case (es, a) => f(a)(es).get } (propTag)
  
  def randomStream[A](ga: Gen[A])(rng: RNG): Stream[A] = {
    val (a, rng1) = ga.generate(rng)
    Stream.cons(a, randomStream(ga)(rng1))
  }
  
  def getMsg[A,E<:Exception](a: A, e: E): String =
    s"exception ${e.getClass()} in test case $a:\n" +
    s"${e.getMessage()}\n" +
    s"${e.getStackTrace.mkString("\n")}"
}