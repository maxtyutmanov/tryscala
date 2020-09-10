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

class P[A] extends Parsers[ParseError, Parser] {
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
  
  
}