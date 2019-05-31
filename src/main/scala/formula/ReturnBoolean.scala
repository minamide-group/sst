package formula

import formula.integer.IntV
import formula.str.StrV

trait ReturnBoolean {
  def strVs: Set[StrV]

  def intVs: Set[IntV]

  def chars: Set[Char]
}
