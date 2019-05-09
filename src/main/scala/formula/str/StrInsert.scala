package formula.str

case class StrInsert(strV : StrV, index : Int, str : String) extends ReturnString {
  override def strVs: Set[StrV] = Set(strV)

  override def chars: Set[Char] = str.toSet
}
