package formula.atomic

import formula.integer.IntV
import formula.re.ReturnRe
import formula.str.StrV

case class StrInRe(left : StrV, right : ReturnRe, not : Boolean) extends Atomic {
  override def strVs: Set[StrV] = Set(left)

  override def intVs: Set[IntV] = Set()

  override def chars: Set[Char] = right.chars
}
