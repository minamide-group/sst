package expression.regex

case class AltExp(r1: RegExp, r2: RegExp) extends RegExp