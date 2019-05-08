package formula
import formula.integer.IntV
import formula.str.StrV

case class Negation(p : Formula) extends Formula{
  override def strVs: Set[StrV] = p.strVs

  override def intVs: Set[IntV] = p.intVs

  override def chars: Set[Char] = p.chars
}
