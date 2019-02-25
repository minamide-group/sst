package trim_test

import deterministic.examples.SSTExamples
import org.scalatest.FlatSpec

class Trim_Test extends FlatSpec {

  val sst = SSTExamples.trimable()

  "trim" should "run" in{
    val used = sst.usedVars
    val nonEmpty = sst.nonEmptyVars

    println(used)
    println(nonEmpty)
  }
}
