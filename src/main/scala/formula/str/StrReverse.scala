package formula.str

case class StrReverse(strV: StrV) extends ReturnString {
  override def strVs: Set[StrV] = Set(strV)

  override def chars: Set[Char] = Set()
}
