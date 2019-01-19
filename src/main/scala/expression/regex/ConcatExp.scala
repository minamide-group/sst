package expression.regex

case class ConcatExp(r1: RegExp, r2: RegExp) extends RegExp