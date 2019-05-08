package formula.re

case class ReStar(re : ReturnRe) extends ReturnRe{
  override def chars: Set[Char] = re.chars
}
