package formula.str

case class StrSubstr (strV : StrV, begin : Int) extends ReturnString{
  override def strVs: Set[StrV] = Set(strV)

  override def chars: Set[Char] = Set()
}
