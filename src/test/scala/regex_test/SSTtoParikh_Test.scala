package regex_test

import deterministic.SSTFactory
import deterministic.boundedcopy.SST
import org.scalatest.FlatSpec

class SSTtoParikh_Test extends FlatSpec{

  "Parikh Image" should "run" in{
    val sst = SSTFactory.getReverseSST()
    SST.getParikhImage(sst).foreach(println)
  }

  "inout length" should "run" in{
    val sst = SSTFactory.getReverseSST()
    SST.getSemiLinearSet(sst).foreach(println)
  }

  "z3" should "run" in{
    val sst = SSTFactory.getReverseSST()
    val str = SST.getZ3Input(3, 6, sst)
    println(str)
  }
}
