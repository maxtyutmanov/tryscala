

package object PropTesting {
  type FailedCase = String
  type SuccessCount = Int
  
  type Rand[+A] = State[RNG,A]
}