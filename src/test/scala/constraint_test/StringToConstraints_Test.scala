package constraint_test

import constraint._
import org.scalatest.FlatSpec

class StringToConstraints_Test extends FlatSpec {


  "test1" should "run" in {

    val ic = "(and (= len4 len0) (= len1 4) )"
    val rg = List("0 aab", "4 bbb")
    val rl = List(
      "2 v(0)",
      "3 v(2) w(abc) v(1)",
      "4 replace aa bb 2")
    val chars = "abc"

    val res = Checker.process(chars, rl, rg, ic, '#', false)

    assert(res)
  }

  "test2" should "run" in {
    val ic = "(and (= len1 len0) (= len1 5) )"
    val rg = List("0 aabaa", "1 bbbaa")
    val rl = List(
      "1 replaceFirst aa bb 0")
    val chars = "abc"

    val res = Checker.process(chars, rl, rg, ic, '#', false)

    assert(res)
  }

  "test3" should "run" in {
    val ic = "(and (= len1 len0) (= len2 len3) )"
    val rg = List("0 aa", "1 ba", "3 a*b")
    val rl = List(
      "2 v(1) v(0)",
      "3 reverse 2")
    val chars = "abc"

    val res = Checker.process(chars, rl, rg, ic, '#', false)

    assert(res)
  }

  "test4" should "run" in {
    val ic = "(> len1 len0)"
    val rg = List()
    val rl = List(
      "1 replace aa b 0")
    val chars = "abc"
    val log = true
    val split = '#'

    val res = Checker.process(chars, rl, rg, ic, split, log)
    assert(!res)
  }
}
