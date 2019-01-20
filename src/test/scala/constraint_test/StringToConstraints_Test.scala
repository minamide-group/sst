package constraint_test

import org.scalatest.FlatSpec
import constraint._

class StringToConstraints_Test extends FlatSpec{


  "program" should "run" in{

    val ic = "(and ( > len4 len0) ( = len1 4 ) )"
    val rg = List("0 ab*")
    val rl = List(
      "2 v(1) v(0)",
      "3 v(2) w(abc) v(1)",
      "4 replace a bb 3")
    val builder = new ConstraintBuilder("abc", rl, rg, ic)
    val res = Checker.process(builder.toConstraints, '#')

    assert(res)
  }
}
