package formula.integer

import formula.str.StrV

trait ReturnInteger {
  def toFormula(map : Map[StrV, Int]) : String

  def intVs : Set[IntV]

  def strVs : Set[StrV]
}
