import PropTesting.Gen
import PropTesting.RNG
import PropTesting.SimpleRNG
import java.util.stream.IntStream
import scala.collection.immutable.HashSet
import PropTesting.SGen
import PropTesting.Prop
import PropTesting.Par
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors


object Program {
  def main(args: Array[String]): Unit = {
    checkFork
  }
  
  private def checkParIdentity(): Unit = {
    val parInt1 = Gen.choose(0, 10).map(Par.unit).map(Par.fork(_))
    val parInt2 = Gen.choose(0, 10).map(Par.unit)
    val parInt = Gen.map2(parInt1, parInt2)((p1, p2) => Par.map2(p1, p2)(_+_))
    val p = PropTesting.forAllPar(parInt)(pi => Par.equal(Par.map(pi)(x => x), pi))("identity func")
    PropTesting.run(p)
  }
  
  private def checkFork(): Unit = {
    val p = PropTesting.forAllPar(Gen.choose(1, 100))(x => {
      val woFork = Par.unit(x)
      val wFork = Par.fork(woFork)
      Par.equal(woFork, wFork)
    })("fork(x) == x")
    
    PropTesting.run(p)
  }
  
  private def proveParUnit(): Unit = {
    val p = PropTesting.forAllPar(Gen.choose(1, 10))(x => {
      val p1 = Par.map(Par.unit(x))(_ + 1)
      val p2 = Par.unit(x + 1)
      Par.equal(p1, p2)
    })("unit")
    
    PropTesting.run(p)
  }
  
  private def testSortedFunc(): Unit = {
    val lists = SGen.listOf(Gen.choose(-1000, 1000))
    val isOrderedProp = PropTesting.forAll(lists)(list => checkOrder(list.sorted))("isOrdered")
    PropTesting.run(isOrderedProp)
  }
  
  @annotation.tailrec
  private def checkOrder(xs: List[Int]): Boolean = xs match {
    case Nil => true
    case h::Nil => true
    case h1::h2::t => (h1 <= h2) && checkOrder(t)
  }
  
  private def testMaxFunc(): Unit = {
    val rng = new SimpleRNG(1)
    val lists = SGen.listOf(Gen.choose(-10000, 10000))
    val isAlwaysDefined = PropTesting.forAll(lists)(l => getMax(l).isDefined)("isAlwaysDefined")
    PropTesting.run(isAlwaysDefined)
    val neLists = SGen.listOf1(Gen.choose(-10000, 10000))
    val isDefinedForNonEmptyLists = PropTesting.forAll(neLists)(l => getMax(l).isDefined)("isDefinedForNonEmptyLists")
    println(isDefinedForNonEmptyLists.run(100, 1000, rng))
    val isGeOtherVals = PropTesting.forAll(lists)(l => l.forall(li => getMax(l).getOrElse(0) >= li))("isGeOtherVals")
    PropTesting.run(isGeOtherVals)
    
    val listsWithSameValues = SGen.listOf(Gen.unit(1200))
    val maxIsSameAsOtherVals = PropTesting.forAll(listsWithSameValues)(l => l.forall(li => li == getMax(l).getOrElse(0)))("isSameAsOtherVals")
    println(maxIsSameAsOtherVals.run(100, 1000, rng))
  }
  
  private def getMax(xs: List[Int]): Option[Int] = xs match {
    case Nil => None
    case _ => Some(xs.max)
  }
  
  private def testRandomThings(): Unit = {
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
    
    val prop1 = PropTesting.forAll(Gen.randStr(lenGen, "abcdefghi2"))(str => isMadeOfAlphabet(str, alphabetHs))("ismadeofalphabet")
    println(prop1.run(1, 100, rng16))
    val prop2 = PropTesting.forAll(Gen.randStr(lenGen, "abcdefghi"))(str => isMadeOfAlphabet(str, alphabetHs))("ismadeofalphabet")
    println(prop2.run(1, 100, rng16))
    
    val listOfAbLetters = SGen.listOf(Gen.boolean.map(isA => if (isA) 'A' else 'B')).map(chrs => chrs.mkString)
    val lenCheck = PropTesting.forAll(listOfAbLetters)(str => str.length() < 10)("lengthCheck");
    val containsOnlyAsAndBs = PropTesting.forAll(listOfAbLetters)(str => str.toCharArray().forall(c => c == 'A' || c == 'B'))("only as and bs")
    val onlyAsAndBsOfLenLt10 = lenCheck && containsOnlyAsAndBs
    
    println(onlyAsAndBsOfLenLt10.run(15, 100, rng16))
  }
  
  private def isMadeOfAlphabet(s: String, alphabet: HashSet[Char]): Boolean = {
    s.foldLeft(true)((acc, c) => alphabet.contains(c) && acc)
  }
}