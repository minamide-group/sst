package formula.re

case class ReConcat(re1: ReturnRe, re2: ReturnRe) extends ReturnRe {
  override def chars: Set[Char] = re1.chars ++ re2.chars
}
