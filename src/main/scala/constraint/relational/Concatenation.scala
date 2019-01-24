package constraint.relational

import constraint.vars.StringVariable

case class Concatenation[Σ](left : StringVariable, list : List[Either[StringVariable, List[Σ]]]) extends RelCons{
  override def getLeftIdx(): Int = left.id
}

