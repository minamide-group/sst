package sstBuilder_test

import constraint.regular.RegCons
import constraint.relational.TransducerConstraint
import constraint.vars.StringVariable
import constraint.{ConstraintBuilder, SSTBuilder}
import deterministic.factory.TransducerFactory
import org.scalatest.FlatSpec

class TransToSST_Test extends FlatSpec{

  val charSet = Set('a', 'b')
  val split = '#'
  val builder = SSTBuilder(charSet, split)
  val cBuilder = ConstraintBuilder("ab", List(), List(), "")
  val factory = TransducerFactory(charSet)

  "t" should "run" in {
    val trans = factory.replaceAll('a', "aba")
    val s4 = StringVariable(4)
    val s3 = StringVariable(3)
    val cons = TransducerConstraint(s4, trans, s3)
    val sst = builder.getOne(cons, Set())

    sst.printDetail
  }

  "tR" should "run" in {
    val trans = factory.replaceAll('a', "aba")
    val s4 = StringVariable(4)
    val s3 = StringVariable(3)
    val cons = TransducerConstraint(s4, trans, s3)

    val r0 = "0 ab*"
    val r2 = "2 b*a"
    val regCons = Set(r0, r2).map(str=> cBuilder.toRegCons(str))

    val sst = builder.getOne(cons, regCons)

    sst.printDetail
  }
}
