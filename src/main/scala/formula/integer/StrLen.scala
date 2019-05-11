package formula.integer

import formula.str.{StrV}

case class StrLen(strV : StrV) extends ReturnInteger{
  override def toFormula(map: Map[StrV, Int]): String = if(map.contains(strV)) "len_"+map(strV) else "len_"+strV.name

  override def intVs: Set[IntV] = Set()

  override def strVs: Set[StrV] = Set(strV)
}
