package constraint.relational

import constraint.vars.{SST_State, SST_Var, StringVariable}
import deterministic.boundedcopy.SST

case class SSTConstraint[Σ](left : StringVariable, right1 : SST[SST_State, Σ, Σ, SST_Var], right2 : StringVariable) extends RelCons{
  override def getLeftIdx(): Int = left.id
}
