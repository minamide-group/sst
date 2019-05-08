package formula.re

case class StrToRe(str : String) extends ReturnRe{
  override def chars: Set[Char] = str.toCharArray.toSet
}
