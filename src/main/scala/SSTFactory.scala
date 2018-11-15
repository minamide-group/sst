import deterministic.SST


object SSTFactory {

  class State(i:Int){
    override def toString: String = "q"+i
  }
  class Variable(i:Int){
    override def toString: String = "x"+i
  }


  def getReverseSST()={
    val q0 = new State(0)
    val q1 = new State(1)
    val q_sink = new State(-1)

    val x0 = new Variable(0)
    val x1 = new Variable(1)

    val delta = Map(
      (q0,'a')->q1,
      (q0,'b')->q1,
      (q1,'a')->q1,
      (q1,'b')->q1
    ).withDefaultValue(q_sink)

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x0), Right('a')),
        x1->List(Right('a'), Left(x1))
      ),
      (q0,'b')->Map(
        x0->List(Left(x0), Right('b')),
        x1->List(Right('b'),Left(x1))
      ),
      (q1,'a')->Map(
        x0->List(Left(x0), Right('a')),
        x1->List(Right('a'), Left(x1))
      ),
      (q1,'b')->Map(
        x0->List(Left(x0), Right('b')),
        x1->List(Right('b'),Left(x1))
      )
    ).withDefaultValue(Map())

    val f = Map(q1->List(Left(x0), Right('#'), Left(x1))).withDefaultValue(List())
    SST(Set(q0,q1,q_sink), q0, Set(x0,x1), delta, eta, f)
  }

  def getHalfSST()={
    val q0 = new State(0)
    val q1 = new State(1)
    val q_sink = new State(-1)

    val x0 = new Variable(0)

    val delta = Map(
      (q0,'a')->q1,
      (q0,'b')->q1,
      (q1,'a')->q0,
      (q1,'b')->q0
    ).withDefaultValue(q_sink)

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x0), Right('a'))
      ),
      (q0,'b')->Map(
        x0->List(Left(x0), Right('b'))
      ),
      (q1,'a')->Map(
        x0->List(Left(x0))
      ),
      (q1,'b')->Map(
        x0->List(Left(x0))
      )
    ).withDefaultValue(Map())

    val f = Map(q0->List(Left(x0)), q1->List(Left(x0))).withDefaultValue(List())
    SST(Set(q0,q1,q_sink), q0, Set(x0), delta, eta, f)
  }

  def getDeletionSST()={
    val q0 = new State(0)
    val q_sink = new State(-1)

    val x0 = new Variable(0)

    val delta = Map(
      (q0,'a')->q0,
      (q0,'b')->q0
    ).withDefaultValue(q_sink)

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x0))
      ),
      (q0,'b')->Map(
        x0->List(Left(x0), Right('b'))
      )
    ).withDefaultValue(Map())

    val f = Map(q0->List( Left(x0) ) ).withDefaultValue(List())
    SST(Set(q0,q_sink), q0, Set(x0), delta, eta, f)
  }

  def getCPSST()={
    val q0 = new State(0)
    val q_sink = new State(-1)

    val x0 = new Variable(0)
    val x1 = new Variable(1)

    val delta = Map(
      (q0,'a')->q0,
      (q0,'b')->q0
    ).withDefaultValue(q_sink)

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x0), Right('a')),
        x1->List(Left(x1), Right('b'))
      ),
      (q0,'b')->Map(
        x0->List(Left(x0), Left(x1)),
        x1->List()
      )
    ).withDefaultValue(Map())

    val f = Map(q0->List(Left(x0))).withDefaultValue(List())
    SST(Set(q0,q_sink), q0, Set(x0,x1), delta, eta, f)
  }
}
