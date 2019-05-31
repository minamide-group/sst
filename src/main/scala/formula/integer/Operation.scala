package formula.integer

import formula.str.StrV

case class Operation(v1: ReturnInteger, v2: ReturnInteger, op: String) extends ReturnInteger {
  override def toFormula(map: Map[StrV, Int]): String = "(" + op + " " + v1.toFormula(map) + " " + v2.toFormula(map) + ")"

  override def intVs: Set[IntV] = v1.intVs ++ v2.intVs

  override def strVs: Set[StrV] = v1.strVs ++ v2.strVs
}
