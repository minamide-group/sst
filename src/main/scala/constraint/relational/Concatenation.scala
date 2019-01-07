package constraint.relational

import constraint.vars.StringVariable

case class Concatenation(left : StringVariable, right1 : StringVariable, right2 : StringVariable) extends RelCons{
  override def getLeftIdx(): Int = left.id
}
