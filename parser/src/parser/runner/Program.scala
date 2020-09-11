package parser.runner

import parser.core.P;

object Program {
  def main(args: Array[String]): Unit = {
    val parsers = new P[String]
    val abcParser = parsers.string("abc")
    val defParser = parsers.string("def")
    val abcOrDefParser = parsers.or(abcParser, defParser)
    val multipleAbcOrDef = parsers.listOfN(3, abcOrDefParser)
    
    println(parsers.run(multipleAbcOrDef)("abcdefabc"))
  }
}