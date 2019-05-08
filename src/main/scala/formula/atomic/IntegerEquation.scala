package formula.atomic

import formula.integer.{IntV, ReturnInteger}
import formula.str.StrV

case class IntegerEquation(left: ReturnInteger, right : ReturnInteger, op : Int) extends Atomic{
  override def strVs: Set[StrV] = left.strVs ++ right.strVs

  override def intVs: Set[IntV] = left.intVs ++ right.intVs

  override def chars: Set[Char] = Set()

  def toFormula(map : Map[StrV, Int]) : String ={
    op match {
      case 1 =>"(=" + " " + left.toFormula(map) + " " + right.toFormula(map) + ")"
      case -1=>"(not (=" + " " + left.toFormula(map) + " " + right.toFormula(map) + "))"
      case 2 =>"(<" + " " + left.toFormula(map) + " " + right.toFormula(map) + ")"
      case -2=>"(>=" + " " + left.toFormula(map) + " " + right.toFormula(map) + ")"
      case 3=> "(>" + " " + left.toFormula(map) + " " + right.toFormula(map) + ")"
      case -3=>"(<=" + " " + left.toFormula(map) + " " + right.toFormula(map) + ")"
    }
  }
}
