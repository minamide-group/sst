package constraint.atomicSL

import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.SST

case class SSTConstraint[Σ](left: Int, sst: SST[SST_State, Σ, Σ, SST_Var], source: Int) extends AtomicSLCons {
  override def getLeftIdx(): Int = left
}
