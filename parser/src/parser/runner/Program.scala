package parser.runner

import parser.core.P._;

object Program {
  def main(args: Array[String]): Unit = {
    val multipleAbcOrDef = listOfN(3, string("abc") | string("def"))
    println(run(multipleAbcOrDef)("abcdefabc"))
    println(run(oneOrMore(string("abc") | string("def"))(0)((a, b) => b + 1))("abcdefabc"))
    
    val p = zeroOrMore(char('a'))(0)((a, cnt) => cnt + 1)
      .map2(oneOrMore(char('b'))(0)((a, cnt) => cnt + 1))((_, _))
      
    println(run(p)("bbb"))
    println(run(p)("aaab"))
  }
}