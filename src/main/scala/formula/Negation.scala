package formula

import formula.integer.IntV
import formula.str.StrV

case class Negation(p: ReturnBoolean) extends ReturnBoolean {
  override def strVs: Set[StrV] = p.strVs

  override def intVs: Set[IntV] = p.intVs

  override def chars: Set[Char] = p.chars
}
