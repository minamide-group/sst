package constraint_test

import org.scalatest.FlatSpec
import constraint._

class StringToConstraints_Test extends FlatSpec{


  "program" should "run" in{
    var builder = new ConstraintBuilder("abc")
    builder.addRegCons("0 ab*") // x0 in {ab*}

    builder.addRelCons_0("3 v(2) w(abc) v(1)")
    builder.addRelCons_1("4 replace a bb 3")

    builder.addIntCons("(and ( > len4 len0 ) ( = len1 4 ) )")

    val (intCons, relCons, regCons, charSet) = builder.toConstraints

    val res = Checker.process(intCons, relCons, regCons, charSet, '#')

    assert(res)
  }
}
