package formula.atomic

import formula.integer.IntV
import formula.str.{ReturnString, StrV}

case class WordEquation(left : StrV, right: ReturnString) extends Atomic {
  override def strVs: Set[StrV] = right.strVs + left

  override def intVs: Set[IntV] = Set()

  override def chars: Set[Char] = right.chars
}
