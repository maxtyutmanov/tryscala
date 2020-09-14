package parser.core

case class ParserState(text: String) {
  def move(charsCount: Int) = {
    ParserState(text.substring(charsCount))
  }
}

trait ParseResult {
  def isSuccess: Boolean
}

case class ParseError(msg: String) extends ParseResult {
  def isSuccess = false
}

case class ParseSuccess[+A](a: A, s: ParserState) extends ParseResult {
  def isSuccess = true
}

case class Parser[+A](run: ParserState => ParseResult) {
  // TODO: if we use flatMap here there'll be an infinite recursion
  def map[B](f: A => B): Parser[B] =
    flatMap(a => P.unit(f(a)))
  
  def map2[B,C](p: => Parser[B])(g: (A, B) => C) =
    P.product(this, p).map(g.tupled)
  
  def flatMap[B](f: A => Parser[B]): Parser[B] = 
    Parser(state => {
      val res = run(state)
      res match {
        case ParseSuccess(a: A, nextState) => f(a).run(nextState)
        case _ => res
      }
    })
  }

object P extends Parsers[ParseError, Parser] {
  def product[A,B](pa: Parser[A], pb: => Parser[B]): Parser[(A,B)] =
    pa.flatMap(a => pb.map(b => (a, b)))
  
  def slice[A](p: Parser[A]): Parser[String] = {
    Parser(state => {
      val res = p.run(state)
      res match {
        case ParseSuccess(a, nextState) => {
          val cLen = state.text.length - nextState.text.length
          val cText = state.text.substring(0, cLen)
          ParseSuccess(cText, nextState)
        }
        case _ => res
      }
    })
  }
  
  def many[A](p: Parser[A]): Parser[List[A]] = {
    p.map2(many(p))(_::_) or unit(Nil)
  }
  
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    p.map2(many(p))(_::_)
  }
  
  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = {
    val state = ParserState(input)
    val result = p.run(state)
    result match {
      case ParseSuccess(a: A, _) => Right(a)
      case pe: ParseError => Left(pe)
    }
  }
  
  def digit(): Parser[Int] = 
    charRange('0', '9').map(chr => (chr - '0'))
  
  def charRange(min: Char, max: Char): Parser[Char] = {
    val charsP = (min.toInt to max.toInt).map(codepoint => char(codepoint.toChar))
    charsP.tail.foldLeft(charsP.head)((p, acc) => p.or(acc))
  }
  
  def unit[A](a: A): Parser[A] = {
    Parser[A](state => ParseSuccess(a, state))
  }
  
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n == 0) unit(Nil)
    else p.map2(listOfN(n - 1, p))(_::_)
  }

  def char(c: Char): Parser[Char] = {
    string(c.toString()).map(_.charAt(0))
  }
  
  def string(s: String): Parser[String] = {
    Parser[String](state => {
      if (state.text.startsWith(s)) ParseSuccess(s, state.move(s.length))
      else ParseError(s"Expected string $s")
    })
  }
  
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = {
    Parser[A](state => {
      val res1 = s1.run(state)
      res1 match {
        case ParseSuccess(_,_) => res1
        case _ => s2.run(state)
      }
    })
  }
}