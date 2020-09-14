package parser.runner

import parser.core.P._;
import parser.core._;

object Program {
  def main(args: Array[String]): Unit = {
    val multipleAbcOrDef = listOfN(3, string("abc") | string("def"))
    println(run(multipleAbcOrDef)("abcdefabc"))
    //println(run(string("abc") | string("def"))("abc"))
    //println(run(many1(string("abc") | string("def")))("abcdefabc"))
    
    val p = string("abc").flatMap(s => string("def").map(x => x.length))
    println(run(p)("abcdef"))
    
    testContextSensitive()
  }
  
  def testContextSensitive(): Unit = {
    // digit followed by that many 'a' characters that it denotes
    val p = digit.flatMap(d => listOfN(d, char('a')))
    println(run(p)("4aaa"))
    println(run(p)("3aaa"))
    println(run(p)("aaa"))
  }
}