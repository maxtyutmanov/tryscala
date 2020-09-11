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
  def map2[B,C](p: Parser[B])(g: (A, B) => C) = {
    Parser(state => {
      val result = this.run(state)
      result match {
        case ParseSuccess(a: A, nextState) => {
          val nextResult = p.run(nextState)
          nextResult match {
            case ParseSuccess(b: B, finalState) => {
              ParseSuccess(g(a, b), finalState)
            }
            case _ => nextResult
          }
        }
        case _ => result
      }
    })
  }
  
  def join[B>:A](p: Parser[List[B]]): Parser[List[A]] = {
    this.map2(p)((a, alist) => a::alist)
  }
}

object P extends Parsers[ParseError, Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = {
    val state = ParserState(input)
    val result = p.run(state)
    result match {
      case ParseSuccess(a: A, _) => Right(a)
      case pe: ParseError => Left(pe)
    }
  }
  
  def oneOrMore[A,B](p: Parser[A])(seed: B)(g: (A, B) => B): Parser[B] = {
    p.map2(zeroOrMore(p)(seed)(g))(g)
  }
  
  def zeroOrMore[A,B](p: Parser[A])(seed: B)(g: (A, B) => B): Parser[B] = {
    Parser[B](state => {
      val res = p.run(state)
      res match {
        case ParseSuccess(a: A, nextState) => {
          val nextParser = zeroOrMore(p)(seed)(g)
          val nextRes = nextParser.run(nextState)
          nextRes match {
            case ParseSuccess(b: B, finalState) => ParseSuccess(g(a, b), finalState)
            case _ => nextRes
          }
        }
        case _ => ParseSuccess(seed, state)
      }
    })
  }
  
  def unit[A](a: A): Parser[A] = {
    Parser[A](state => ParseSuccess(a, state))
  }
  
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) Parser(s => ParseSuccess(Nil, s))
    else p.join(listOfN(n - 1, p))
  }

  def char(c: Char): Parser[Char] = {
    Parser[Char](state => {
      if (state.text.startsWith(c.toString())) ParseSuccess(c, state.move(1))
      else ParseError(s"Expected character $c")
    })
  }
  
  def string(s: String): Parser[String] = {
    Parser[String](state => {
      if (state.text.startsWith(s)) ParseSuccess(s, state.move(s.length))
      else ParseError(s"Expected string $s")
    })
  }
  
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = {
    Parser[A](state => {
      val res1 = s1.run(state)
      res1 match {
        case ParseSuccess(_,_) => res1
        case _ => s2.run(state)
      }
    })
  }
}