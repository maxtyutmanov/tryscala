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
  def map[B](f: A => B): Parser[B] = {
    Parser[B](state => {
      val res = run(state)
      res match {
        case ParseSuccess(a: A, nextState) => ParseSuccess(f(a), nextState)
        case _ => res
      }
    })
  }
  
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
  
  def range[A,B](min: Int, take: Option[Int], p: Parser[A])(seed: B)(g: (A, B) => B): Parser[B] = {
    if (min > 0) {
      val nextP = range(min - 1, take.map(_ - 1), p)(seed)(g)
      p.map2(nextP)(g)
    }
    else {
      // check if we hit the maximum number of occurences
      val noMore = take.map(_ <= 0).getOrElse(false)
      if (noMore) unit(seed)
      else {
        Parser[B](state => {
          val res = p.run(state)
          res match {
            case ParseSuccess(a: A, nextState) => {
              val nextParser = range(min, take.map(_ - 1), p)(seed)(g)
              val nextRes = nextParser.run(nextState)
              nextRes match {
                case ParseSuccess(b: B, finalState) => ParseSuccess(g(a, b), finalState)
                case _ => nextRes
              }
            }
            // parseerror is actually a success here, because we expect 
            // _zero_ or more occurences, and this case covers _zero_
            case _ => ParseSuccess(seed, state)
          }
        })
      }
    }
  }
  
  def oneOrMore[A,B](p: Parser[A])(seed: B)(g: (A, B) => B): Parser[B] = {
    range(1, None, p)(seed)(g)
  }
  
  def zeroOrMore[A,B](p: Parser[A])(seed: B)(g: (A, B) => B): Parser[B] = {
    range(0, None, p)(seed)(g)
  }
  
  def unit[A](a: A): Parser[A] = {
    string("").map(_ => a)
  }
  
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    range(n, Some(n), p)(Nil:List[A])(_::_)
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