package builderTest

import builder.FormulaBuilder
import org.scalatest.FlatSpec

class Token_Test extends FlatSpec {


  val lines = List(
    ";; activate model generation",
    "",
    "(declare-fun x () Int)",
    "(declare-fun y1 () String)",
    "(declare-fun y2 () String)",
    "(declare-fun z () Int)",
    "",
    "(assert (= x z))",
    "(assert (not (= y1 y2)))",
    "(assert (str.in.re y1 (str.to.re \"a b c\")))",
    "",
    "(check-sat)"
  ).mkString

  "tokens" should "run" in {
    val fb = FormulaBuilder(lines)
    fb.getTokens(lines).foreach(println)
  }

  "parse" should "run" in {
    val fb = FormulaBuilder(lines)
    val tokens = fb.getTokens(lines)
    val (temp1, strV1, intV1) = fb.parseDeclareFun(tokens, Set(), Set())
    println(strV1)
    println(intV1)
    println(temp1)
    println("==========")
    val (temp2, strV2, intV2) = fb.parseDeclareFun(temp1, strV1, intV1)
    println(strV2)
    println(intV2)
    println(temp2)
    println("==========")
    val (temp3, strV3, intV3) = fb.parseDeclareFun(temp2, strV2, intV2)
    println(strV3)
    println(intV3)
    println(temp3)
    println("==========")
    val (temp4, strV4, intV4) = fb.parseDeclareFun(temp3, strV3, intV3)
    println(strV4)
    println(intV4)
    println(temp4)
    println("==========")
    val (temp5, f1) = fb.parseAssert(temp4, strV4, intV4)
    println(f1)
    println(temp5)
    println("==========")
    val (temp6, f2) = fb.parseAssert(temp5, strV4, intV4)
    println(f2)
    println(temp6)
    println("==========")
    val (temp7, f3) = fb.parseAssert(temp6, strV4, intV4)
    println(f3)
    println(temp7)
    println("==========")
  }

  "toFormula" should "run" in {
    val fb = FormulaBuilder(lines)
    println(fb.output)
  }
}
