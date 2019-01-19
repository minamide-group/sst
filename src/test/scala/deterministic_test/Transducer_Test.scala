package deterministic_test

import deterministic.factory.TransducerFactory
import org.scalatest.FlatSpec

class Transducer_Test extends FlatSpec{

  "replace" should "run" in {
    val transducer = TransducerFactory(Set('a', 'b', 'c')).replace('a', "bbb")
    val res= transducer.trans("bacaaa".toList)(transducer.initialStates)._2.mkString
    assert(res == "bbbbcaaa")
  }
}
