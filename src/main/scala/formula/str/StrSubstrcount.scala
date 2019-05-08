package formula.str

case class StrSubstrcount (strV : StrV, begin : Int, count : Int) extends ReturnString{
  override def strVs: Set[StrV] = Set(strV)

  override def chars: Set[Char] = Set()
}
