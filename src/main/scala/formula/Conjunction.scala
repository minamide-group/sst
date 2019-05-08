package formula

import formula.integer.IntV
import formula.str.StrV

case class Conjunction(p1 : Formula, p2 : Formula) extends Formula{
  override def strVs: Set[StrV] = p1.strVs ++ p2.strVs

  override def intVs: Set[IntV] = p1.intVs ++ p2.intVs

  override def chars: Set[Char] = p1.chars ++ p2.chars
}
