package deterministic_test

import deterministic.boundedcopy.SST
import deterministic.examples.SSTExamples
import org.scalatest.FlatSpec

class SSTtoParikh_Test extends FlatSpec{

  "Parikh Image" should "run" in{
    val sst = SSTExamples.getReverseSST()
    SST.getParikhImage(sst).foreach(println)
  }

}
