package regex_test

import deterministic.factory.DFAFactory
import expression.regex._
import org.scalatest.FlatSpec

class RegToDFA_Test extends FlatSpec{

  // [ (a|b|c)* ]
  val regex = ConcatExp(
    CharExp('['),
    ConcatExp(
      StarExp(
        AltExp(
          CharExp('a'),
          AltExp(
            CharExp('b'),
            CharExp('c')))),
      CharExp(']')
    )
  )

  val dfa = DFAFactory().RegToDFA(regex)
  val r= scala.util.Random

  "program" should "run" in {
    assert(dfa.process("[]"))
    assert(dfa.process("[bc]"))
    assert(dfa.process("[aabc]"))
    assert(dfa.process("[aabcacacaca]"))
    assert(!dfa.process("[f]"))
    assert(!dfa.process("dda"))
    assert(!dfa.process(""))
  }
}
