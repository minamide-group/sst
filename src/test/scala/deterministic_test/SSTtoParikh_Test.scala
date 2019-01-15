package deterministic_test

import deterministic.boundedcopy.SST
import deterministic.factories.SSTFactory
import org.scalatest.FlatSpec

class SSTtoParikh_Test extends FlatSpec{

  "Parikh Image" should "run" in{
    val sst = SSTFactory.getReverseSST()
    SST.getParikhImage(sst).foreach(println)
  }

}
