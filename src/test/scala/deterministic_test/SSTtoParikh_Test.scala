package deterministic_test

import deterministic.examples.SSTExamples
import org.scalatest.FlatSpec

class SSTtoParikh_Test extends FlatSpec{

  "Parikh Image" should "run" in{
    val sst = SSTExamples.threeOrFive()
    //val mt = sst.toMapTransducer
    //mt.rename.print

    sst.toParikhImage.foreach(println)
  }

}
