package formula.str

case class StrConcat(list : List[Either[StrV, String]]) extends ReturnString{
  override def strVs: Set[StrV] = list.collect{case Left(x) =>x}.toSet

  override def chars: Set[Char] = list.collect{case Right(str)=>str}.flatMap(s=>s.toCharArray).toSet
}
