package parser.tests

import parser.core._
import PropTesting.Gen

object TestRunner {
  def main(args: Array[String]) = {
    runMapLaw
    runUnitLaw
    runSliceEntireStrLaw
  }
  
  def runSliceEntireStrLaw: Unit = {
    val prop = Laws.sliceEntireStrLaw()
    PropTesting.run(prop)
  }
  
  def runMapLaw(): Unit = {
    val numericStrings = Gen.randStr(Gen.choose(0, 50), "0123456789")
    val nonNumericStrings = Gen.randStr(Gen.choose(0, 50), "qwertyuiop")
    val inputStrings = Gen.weighted((numericStrings, 0.5), (nonNumericStrings, 0.5))
    val prop = Laws.mapLaw(digitParser)(inputStrings)
    PropTesting.run(prop)
  }
  
  def runUnitLaw(): Unit = {
    val inputStrings = Gen.randStr(Gen.choose(0, 50), "0123456789qwertyuiop")
    val inputInts = Gen.choose(10, 1000)
    val prop = Laws.unitLaw(inputInts, inputStrings)
    PropTesting.run(prop)
  }
  
  def digitParser(): Parser[Int] = {
    Parser[Int](state => {
      if (state.text.length() > 0) {
        val chr = state.text.charAt(0)
        if ('0' <= chr && chr <= '9') ParseSuccess(chr - '0', state.move(1))
        else ParseError(s"Digit expected, but found $chr")
      }
      else ParseError("Unexpected end of input")
    })
  }
}