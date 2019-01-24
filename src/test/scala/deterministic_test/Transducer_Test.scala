package deterministic_test

import deterministic.factory.TransducerFactory
import org.scalatest.FlatSpec

class Transducer_Test extends FlatSpec{

  "replace" should "run" in {
    val transducer = TransducerFactory(Set('a', 'b', 'c')).replaceFirst('a', "bbb")
    val res= transducer.trans("bacaaa".toList)(transducer.s0)._2.mkString
    assert(res == "bbbbcaaa")
  }
}
