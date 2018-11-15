package nondeterministic

import org.scalatest.FlatSpec

import scala.util.Random

class NFA_test extends FlatSpec{

  class State(i:Int){
    override def toString: String = "q"+i
  }

  val q0 = new State(0)
  val q1 = new State(0)
  val q2 = new State(0)

  val delta = Map(
    (q0,'a')->Set(q1),
    (q1,'c')->Set(q2),
    (q2,'d')->Set(q1),
    (q1,'b')->Set(q0)
  ).withDefaultValue(Set())

  val nfa = NFA(Set(q0,q1,q2), q0, delta, Set(q2))
  val regex = nfa.toRegEx.get

  def getRandomString(length: Int, chars: List[Char]):String = {
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (_ <- 0 to length-1)
      sb.append(chars(r.nextInt(chars.size)))
    sb.toString
  }

  val r= new Random
  "nfa" should "run" in {
    for(_<- 0 to 5000){
      val str = getRandomString(r.nextInt(1000), List('a','b','c','d'))
      assert(nfa.process(str)._1 == str.matches(regex))
    }
  }
}

