package regex_test

import deterministic.factory.DFAFactory
import org.scalatest.FlatSpec

class ParseRegex_Test extends FlatSpec{

  "emptyString" should "run" in {
    myAssert("", List('a', 'b', 'c'), 100)
  }

  "concat" should "run" in {
    myAssert("abc", List('a', 'b', 'c'), 100)
  }

  "alt" should "run" in {
    myAssert("a|b|c", List('a', 'b', 'c'), 100)
  }

  "star" should "run" in {
    myAssert("a*bcd*", List('a', 'b', 'c', 'd'), 100)
  }

  "p1" should "run" in {
    myAssert("(ab)*c", List('a', 'b', 'c'), 100)
  }

  "p2" should "run" in {
    myAssert("( a | b ) * c", List('a', 'b', 'c'), 100)
  }

  "p3" should "run" in {
    myAssert("(a|b|d*)c", List('a', 'b', 'c', 'd'), 100)
  }

  "p4" should "run" in {
    myAssert("(a|(b|e)|d*)c", List('a', 'b', 'c', 'd', 'e'), 100)
  }

  "p5" should "run" in {
    myAssert("()|(a|(b|e)|d*)c", List('a', 'b', 'c', 'd', 'e'), 100)
  }

  val r = scala.util.Random

  val factory = DFAFactory()

  def getRandomString(length: Int, chars: List[Char]): String = {
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (_ <- 0 to length - 1)
      sb.append(chars(r.nextInt(chars.size)))
    sb.toString
  }

  def myAssert(regex : String, chars : List[Char], testTime : Int): Unit ={
    val dfa = factory.RegToDFA(factory.parseReg(regex))
    for(_  <- 0 to testTime){
      val length = r.nextInt(100)
      val input = getRandomString(length, chars)
      val res = dfa.process(input)
      val expect = input.matches(regex)
      assert(res==expect)
    }
  }
}
