package formula.str

case class StrV(name: String) extends ReturnString {
  override def strVs: Set[StrV] = Set(this)

  override def chars: Set[Char] = Set()
}
