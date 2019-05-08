package formula.integer
import formula.str.StrV

case class IntC(value : Int) extends ReturnInteger{
  override def toFormula(map: Map[StrV, Int]): String = value.toString

  override def intVs: Set[IntV] = Set()

  override def strVs: Set[StrV] = Set()
}
