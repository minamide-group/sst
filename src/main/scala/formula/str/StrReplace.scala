package formula.str

case class StrReplace(strV: StrV, pattern: String, replacement: String) extends ReturnString {
  override def strVs: Set[StrV] = Set(strV)

  override def chars: Set[Char] = pattern.toSet ++ replacement.toSet
}
