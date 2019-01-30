package sstBuilder_test

import constraint.relational.{Concatenation, SSTConstraint}
import constraint.vars.StringVariable
import constraint.{ConstraintBuilder, SSTBuilder}
import deterministic.factory.SSTFactory
import org.scalatest.FlatSpec

class Integrate_Test extends FlatSpec{

  val charSet = Set('a', 'b')
  val split = '#'
  val builder = SSTBuilder(charSet, split)
  val cBuilder = ConstraintBuilder("ab", List(), List(), "")
  val factory = SSTFactory(charSet)

  "i" should "run" in{
    val s = List.range(0,5).map(i=>StringVariable(i))
    val cons = List(
      Concatenation(s(3), List(Left(s(0)), Left(s(2)))),
      SSTConstraint(s(4), factory.reverse, s(3))
    )

    val reg = List("0 ab*", "4 b*a").map(t=> cBuilder.toRegCons(t))

    val sstList = builder.constraintsToSSTs(cons, reg.toSet)

    val x = builder.composeAndCheck(sstList)

    if(x.nonEmpty){
      val sst = x.get
      sst.print
    }
  }
}
