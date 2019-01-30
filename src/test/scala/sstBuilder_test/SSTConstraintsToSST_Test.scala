package sstBuilder_test

import constraint.relational.{SSTConstraint}
import constraint.vars.StringVariable
import constraint.{ConstraintBuilder, SSTBuilder}
import deterministic.factory.SSTFactory
import org.scalatest.FlatSpec

class SSTConstraintsToSST_Test extends FlatSpec{

  val charSet = Set('a', 'b')
  val split = '#'
  val builder = SSTBuilder(charSet, split)
  val cBuilder = ConstraintBuilder("ab", List(), List(), "")
  val factory = SSTFactory(charSet)

  "s" should "run" in {
    val sst0 = factory.reverse
    val s4 = StringVariable(4)
    val s3 = StringVariable(3)
    val cons = SSTConstraint(s4, sst0, s3)

    val sst = builder.getOne(cons, Set())
    sst.printDetail
  }

}
