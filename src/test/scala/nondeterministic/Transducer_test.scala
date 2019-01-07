package nondeterministic

import org.scalatest.FlatSpec
import scalaz.Monoid

class Transducer_test extends FlatSpec{

  class State(i:Int){
    override def toString: String = "q"+i
  }

  "transducer on Int" should "transfer" in {
    implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
      def append(f1: Int, f2: => Int):Int = f1 + f2
      def zero: Int = 0
    }

    val q0 = new State(0)

    val delta = Set(
      (q0, 'a', q0, 1),
      (q0, 'a', q0, 2)
    )

    val transducer1 = Transducer(Set(q0), Set(q0), delta, Set(q0))

    val resultSet = transducer1.trans("aaa".toList)(q0)

    assert(resultSet.size==4)
    assert(resultSet(q0,3))
    assert(resultSet(q0,4))
    assert(resultSet(q0,5))
    assert(resultSet(q0,6))

    val resultSet1 = transducer1.trans("abc".toList)(q0)
    assert(resultSet1.isEmpty)
  }

  "transducer on String" should "transfer" in {
    implicit def strMonoid: Monoid[String] = new Monoid[String] {
      def append(f1: String, f2: => String):String = f1 + f2
      def zero: String = ""
    }

    val q0 = new State(0)

    val delta = Set(
      (q0, 'a', q0, "A"),
      (q0, 'a', q0, "AA")
    )
    val transducer1 = Transducer(Set(q0), Set(q0), delta, Set(q0))

    val resultSet = transducer1.trans("aaa".toList)(q0)

    assert(resultSet.size==4)
    assert(resultSet(q0,"AAA"))
    assert(resultSet(q0,"AAAA"))
    assert(resultSet(q0,"AAAAA"))
    assert(resultSet(q0,"AAAAAA"))

    val resultSet1 = transducer1.trans("abc".toList)(q0)
    assert(resultSet1.isEmpty)
  }

}
