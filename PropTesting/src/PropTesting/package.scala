

package object PropTesting {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Rand[+A] = State[RNG,A]
  
  def forAll[A](ga: Gen[A])(f: A => Boolean): Prop = {
    val run = (numCases: TestCases, rng: RNG) => {
       val (listOfA, rng2) = Gen.listOfN(numCases, ga).generate(rng)
       val falseIx = listOfA.indexWhere(a => !f(a))
       if (falseIx >= 0) {
         // falsified
         Falsified(listOfA(falseIx).toString(), falseIx)
       }
       else {
         Passed
       }
    }
    
    Prop(run)
  }
}