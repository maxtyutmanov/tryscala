import PropTesting.Gen
import PropTesting.RNG
import PropTesting.SimpleRNG


object Program {
  def main(args: Array[String]): Unit = {
    val rng = new SimpleRNG(2)
    val ((x, y), rng2) = Gen.pairInRange(0, 100).generate(rng)
    val ((x2, y2), rng3) = Gen.pairInRange(0, 100).generate(rng2)
    val ((x3, y3), rng4) = Gen.pairInRange(0, 100).generate(rng3)
    val (o1, rng5) = Gen.option(Gen.pairInRange(0, 200)).generate(rng4)
    val (o2, rng6) = Gen.option(Gen.pairInRange(0, 200)).generate(rng5)
    val (o3, rng7) = Gen.option(Gen.pairInRange(0, 200)).generate(rng6)
    val (o4, rng8) = Gen.option(Gen.pairInRange(0, 200)).generate(rng7)
    val (s1, rng9) = Gen.randStr(10, "abcdefghij1234567890").generate(rng8)
    val (s2, rng10) = Gen.randStr(15, "abcdefghij123567890").generate(rng9)
    
    println((x, y))
    println((x2, y2))
    println((x3, y3))
    println(o1)
    println(o2)
    println(o3)
    println(o4)
    println(s1)
    println(s2)
  }
}