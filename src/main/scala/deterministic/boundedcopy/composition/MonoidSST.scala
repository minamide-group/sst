package deterministic.boundedcopy.composition

import deterministic.boundedcopy.SST

case class MonoidSST[Q, A, B, X, Y](
                                     sst: SST[Q, A, Map[Y, List[Either[Y, B]]], X],
                                     vars2: Set[Y],
                                     final2: Map[Q, List[Either[Y, B]]]) {

  def trimStates: MonoidSST[Int, A, B, X, Y] = {
    val msst0 = rename
    MonoidSST(msst0.sst.trimStates, msst0.vars2, msst0.final2)
  }

  private def rename: MonoidSST[Int, A, B, X, Y] = {
    val sMap = sst.states.zipWithIndex.toMap

    val states = sst.states.map(sMap(_))
    val s0 = sMap(sst.s0)
    val delta = sst.δ.map(t => (sMap(t._1._1), t._1._2) -> sMap(t._2))
    val eta = sst.η.map(t => (sMap(t._1._1), t._1._2) -> t._2)
    val f = sst.f.map(t => sMap(t._1) -> t._2)

    val sst_r = SST(states, s0, sst.vars, delta, eta, f)

    val final2_r = final2.map(t => sMap(t._1) -> t._2)

    MonoidSST(sst_r, vars2, final2_r)
  }

  def trimVars: MonoidSST[Q, A, B, Int, Y] = {
    val msst0 = renameX
    MonoidSST(msst0.sst.trimVars, msst0.vars2, msst0.final2)
  }

  private def renameX: MonoidSST[Q, A, B, Int, Y] = {
    val xMap = sst.vars.zipWithIndex.toMap
    val vars = sst.vars.map(xMap(_))
    val eta = sst.η.map(t => t._1 -> t._2.map(r =>
      xMap(r._1) -> r._2.collect {
        case Left(x) => Left(xMap(x))
        case Right(a) => Right(a)
      }
    ))
    val f = sst.f.map(t => t._1 -> t._2.collect {
      case Left(x) => Left(xMap(x))
      case Right(a) => Right(a)
    })

    val sst_r = SST(sst.states, sst.s0, vars, sst.δ, eta, f)

    MonoidSST(sst_r, vars2, final2)
  }
}
