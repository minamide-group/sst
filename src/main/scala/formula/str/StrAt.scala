package formula.str

case class StrAt (strV: StrV, idx : Int) extends ReturnString{
  override def strVs: Set[StrV] = Set(strV)

  override def chars: Set[Char] = Set()
}
