package sstBuilder_test

import constraint.regular.RegCons
import constraint.{ConstraintBuilder, SSTBuilder}
import constraint.relational.Concatenation
import constraint.vars.StringVariable
import org.scalatest.{FlatSpec, PrivateMethodTester}

class ConcatToSST_Test extends FlatSpec with PrivateMethodTester{

  val charSet = Set('a', 'b')
  val split = '#'
  val builder = SSTBuilder(charSet, split)
  val cBuilder = ConstraintBuilder("ab", List(), List(), "")

  "c" should "run" in{
    val s4 = StringVariable(4)
    val s0 = StringVariable(0)
    val s2 = StringVariable(2)

    val concat = Concatenation(s4, List(Left(s0), Right("aba".toList), Left(s2) ))
    val sst = builder.getOne(concat, Set())

    sst.printDetail
  }

  "cR" should "run" in{
    val s4 = StringVariable(4)
    val s0 = StringVariable(0)
    val s2 = StringVariable(2)

    val concat = Concatenation(s4, List(Left(s0), Right("aba".toList), Left(s2) ))

    val r0 = "0 ab*"
    val r3 = "3 b*a"
    val regCons = Set(r0, r3).map(str=> cBuilder.toRegCons(str))

    val sst = builder.getOne(concat, regCons)

    sst.printDetail
  }
}
