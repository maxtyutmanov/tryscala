package parser.tests

import parser.core._;
import parser.core.P;
import PropTesting._;

object Laws {
  def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String])(label: String): Prop =
    forAll(in)(s => P.run(p1)(s) == P.run(p2)(s))(label)

  def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
    equal(p, p.map(a => a))(in)("map identity")
    
  def unitLaw[A](a: Gen[A], in: Gen[String]): Prop = 
    forAll(in)(s => P.run(P.unit(a))(s) == Right(a))("unit")
}