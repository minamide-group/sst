package constraint.relational

import constraint.vars.StringVariable

case class ConstString(left : StringVariable, right : String) extends RelCons{
  override def getLeftIdx(): Int = left.id
}
