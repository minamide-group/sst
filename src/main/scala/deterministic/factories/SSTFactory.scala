package deterministic.factories

import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.SST

object SSTFactory {

  def getReverseSST() = {
    val q0 = SST_State(0, "r")
    val q1 = SST_State(1, "r")
    val q_sink = SST_State(-1, "r")

    val x0 = SST_Var(0, "r")
    val x1 = SST_Var(1, "r")

    val delta = Map(
      (q0, 'a') -> q1,
      (q0, 'b') -> q1,
      (q1, 'a') -> q1,
      (q1, 'b') -> q1
    ).withDefaultValue(q_sink)

    val eta = Map(
      (q0, 'a') -> Map(
        x0 -> List(Left(x0), Right('a')),
        x1 -> List(Right('a'), Left(x1))
      ),
      (q0, 'b') -> Map(
        x0 -> List(Left(x0), Right('b')),
        x1 -> List(Right('b'), Left(x1))
      ),
      (q1, 'a') -> Map(
        x0 -> List(Left(x0), Right('a')),
        x1 -> List(Right('a'), Left(x1))
      ),
      (q1, 'b') -> Map(
        x0 -> List(Left(x0), Right('b')),
        x1 -> List(Right('b'), Left(x1))
      )
    ).withDefaultValue(Map())

    val f = Map(q1 -> List(Left(x0), Right('#'), Left(x1))).withDefaultValue(List())
    SST(Set(q0, q1, q_sink), q0, Set(x0, x1), delta, eta, f)
  }

  def getHalfSST() = {
    val q0 = SST_State(0, "h")
    val q1 = SST_State(1, "h")
    val q_sink = SST_State(-1, "h")

    val x0 = SST_Var(0, "h")

    val delta = Map(
      (q0, 'a') -> q1,
      (q0, 'b') -> q1,
      (q1, 'a') -> q0,
      (q1, 'b') -> q0
    ).withDefaultValue(q_sink)

    val eta = Map(
      (q0, 'a') -> Map(
        x0 -> List(Left(x0), Right('a'))
      ),
      (q0, 'b') -> Map(
        x0 -> List(Left(x0), Right('b'))
      ),
      (q1, 'a') -> Map(
        x0 -> List(Left(x0))
      ),
      (q1, 'b') -> Map(
        x0 -> List(Left(x0))
      )
    ).withDefaultValue(Map())

    val f = Map(q0 -> List(Left(x0)), q1 -> List(Left(x0))).withDefaultValue(List())
    SST(Set(q0, q1, q_sink), q0, Set(x0), delta, eta, f)
  }

  def getDeletionSST() = {
    val q0 = SST_State(0, "d")
    val q_sink = SST_State(-1, "d")

    val x0 = SST_Var(0, "d")

    val delta = Map(
      (q0, 'a') -> q0,
      (q0, 'b') -> q0
    ).withDefaultValue(q_sink)

    val eta = Map(
      (q0, 'a') -> Map(
        x0 -> List(Left(x0))
      ),
      (q0, 'b') -> Map(
        x0 -> List(Left(x0), Right('b'))
      )
    ).withDefaultValue(Map())

    val f = Map(q0 -> List(Left(x0))).withDefaultValue(List())
    SST(Set(q0, q_sink), q0, Set(x0), delta, eta, f)
  }

  def getCPSST() = {
    val q0 = SST_State(0, "c")
    val q_sink = SST_State(-1, "c")

    val x0 = SST_Var(0, "c")
    val x1 = SST_Var(1, "c")

    val delta = Map(
      (q0, 'a') -> q0,
      (q0, 'b') -> q0
    ).withDefaultValue(q_sink)

    val eta = Map(
      (q0, 'a') -> Map(
        x0 -> List(Left(x0), Right('a')),
        x1 -> List(Left(x1), Right('b'))
      ),
      (q0, 'b') -> Map(
        x0 -> List(Left(x0), Left(x1)),
        x1 -> List()
      )
    ).withDefaultValue(Map())

    val f = Map(q0 -> List(Left(x0))).withDefaultValue(List())
    SST(Set(q0, q_sink), q0, Set(x0, x1), delta, eta, f)
  }

}
