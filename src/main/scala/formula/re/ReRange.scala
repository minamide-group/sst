package formula.re

case class ReRange (begin : Char, end : Char) extends ReturnRe {
  override def chars: Set[Char] = List.range(begin, (end+1).toChar).toSet
}
