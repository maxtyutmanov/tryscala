import PropTesting.Gen
import PropTesting.RNG
import PropTesting.SimpleRNG
import java.util.stream.IntStream
import scala.collection.immutable.HashSet


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
    val (s1, rng9) = Gen.randStr(Gen.unit(10), "abcdefghij1234567890").generate(rng8)
    val (s2, rng10) = Gen.randStr(Gen.unit(15), "abcdefghij123567890").generate(rng9)
    val lenGen1 = Gen.choose(1, 10)
    val lenGen2 = Gen.choose(40, 50)
    val lenGen = Gen.weighted((lenGen1, 10), (lenGen2, 20))
    val (s3, rng11) = Gen.randStr(lenGen, "abcdefghij1234567890").generate(rng10)
    val (s4, rng12) = Gen.randStr(lenGen, "abcdefghij1234567890").generate(rng11)
    val (s5, rng13) = Gen.randStr(lenGen, "abcdefghij1234567890").generate(rng12)
    val (s6, rng14) = Gen.randStr(lenGen, "abcdefghij1234567890").generate(rng13)
    val (s7, rng15) = Gen.randStr(lenGen, "abcdefghij1234567890").generate(rng14)
    val (s8, rng16) = Gen.randStr(lenGen, "abcdefghij1234567890").generate(rng15)
    
    println((x, y))
    println((x2, y2))
    println((x3, y3))
    println(o1)
    println(o2)
    println(o3)
    println(o4)
    println(s1)
    println(s2)
    println(s3)
    println(s4)
    println(s5)
    println(s6)
    println(s7)
    println(s8)
    
    val alphabet = "abcdefghi"
    val alphabetHs = HashSet(alphabet.toArray : _*)
    
    val prop = PropTesting.forAll(Gen.randStr(lenGen, "abcdefghi1"))(str => isMadeOfAlphabet(str, alphabetHs))
    println(prop.run(100, rng16))
  }
  
  def isMadeOfAlphabet(s: String, alphabet: HashSet[Char]): Boolean = {
    s.foldLeft(true)((acc, c) => alphabet.contains(c) && acc)
  }
}