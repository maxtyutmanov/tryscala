package parser.tests

import parser.core._;
import parser.core.P;
import PropTesting._;
import parser.core.P._;

object Laws {
  def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String])(label: String): Prop =
    forAll(in)(s => P.run(p1)(s) == P.run(p2)(s))(label)
    
  def sliceEntireStrLaw[A](): Prop = {
    val in = Gen.randStr(Gen.choose(1, 100), "ab")
    val parser = many(char('a') | char('b'))
    forAll(in)(s => {
      val left = P.run(slice(parser))(s) 
      val res = left == Right(s)
      if (!res) println(left)
      res
    })("slice entire str")
  }

  def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
    equal(p, p.map(a => a))(in)("map identity")
    
  def unitLaw[A](a: Gen[A], in: Gen[String]): Prop = 
    forAll(in)(s => P.run(P.unit(a))(s) == Right(a))("unit")
}