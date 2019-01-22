package regex_test

import constraint.vars.FAState
import deterministic.factory.DFAFactory
import nondeterministic.NFA
import org.scalatest.FlatSpec

class RemoveEps_Test extends FlatSpec{

  "program" should "run" in{
    val factory = DFAFactory(Set('a'))
    val eps = factory.Eps
    val ch = factory.Ch
    val s = List.range(0, 7).map(i=>FAState(i))

    val nfa0 : NFA[FAState, factory.C]= NFA(
      s.toSet,
      s(0),
      Map(
        (s(0), eps) -> Set(s(1), s(3)),
        (s(1), ch('a')) -> Set(s(2)),
        (s(2), ch('a')) -> Set(s(1)),
        (s(3), ch('a')) -> Set(s(4)),
        (s(4), ch('a')) -> Set(s(5)),
        (s(5), ch('a')) -> Set(s(3)),
        (s(3), eps) -> Set(s(6))
      ),
      Set(s(1),s(3))
    )

    val nfa1 = factory.removeEps(nfa0)

    val r = new scala.util.Random
    for(_ <- 0 to 100){
      val length = r.nextInt(100)
      val input = List.range(0, length).map(_=>'a')
      val res = nfa1.process(input)
      val expected = (length%2==0 || length%3==0)
      assert(res==expected)
    }
  }

}
