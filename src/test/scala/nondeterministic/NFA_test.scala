package nondeterministic


object NFA_test extends App{

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
  println(nfa.toRegExp)
}

