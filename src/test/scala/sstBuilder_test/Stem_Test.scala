package sstBuilder_test

import constraint.SSTBuilder
import org.scalatest.FlatSpec

class Stem_Test extends FlatSpec{

  val charSet = Set('a', 'b')
  val split = '#'
  val builder = SSTBuilder(charSet, split)

  "stem" should "run" in{
    val num = 4
    val sst = builder.getStem(num, "sst")
    assert(sst.states.size==num+1)
    assert(sst.vars.size==num)
    sst.printDetail
  }
}
