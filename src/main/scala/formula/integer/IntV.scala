package formula.integer

import formula.str.StrV

case class IntV(name : String) extends ReturnInteger{
  override def toFormula(map : Map[StrV, Int]): String = name

  override def intVs: Set[IntV] = Set(this)

  override def strVs: Set[StrV] = Set()
}
