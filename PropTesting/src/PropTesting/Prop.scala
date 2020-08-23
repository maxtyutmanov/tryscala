package PropTesting

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case object Proved extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount, failedPropTag: String) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(other: Prop) = {
    Prop((maxSize, testCases, rng) => {
      Stream(this, other)
        .map(prop => prop.run(maxSize, testCases, rng))
        .find(res => res.isFalsified)
        .getOrElse(Passed)
    })
  }
  
  def ||(other: Prop) = {
    Prop((maxSize, testCases, rng) => {
      val results = Stream(this, other)
        .map(prop => prop.run(maxSize, testCases, rng))
        .toList
      
      if (results.forall(res => res.isFalsified)) results.head
      else Passed
    })
  }
}

object Prop {
  def check(p: => Boolean, tag: String = ""): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("(n/a)", 0, tag)
  }
}