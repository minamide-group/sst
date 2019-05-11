package builder

import formula.atomic.IntegerEquation
import formula.integer.{IntV, StrLen}
import formula.str.StrV

case class Z3InputBuilder(intCons: List[IntegerEquation],
                          semiLinear: Set[(Map[Int, Int], Set[Map[Int, Int]])],
                          nameToIdx: Map[StrV, Int],
                          getModel : Boolean
                         ) {

  def output: String = {
    getDeclare + getLength() + getIntegerCons + "(check-sat)" + (if(getModel) "\n(get-model)" else "")
  }

  val strVs : Set[StrV]= intCons.flatMap(e => e.strVs).toSet
  val intVs : Set[IntV]= intCons.flatMap(e => e.intVs).toSet
  val m0: Map[Int, Int] = nameToIdx.map(t => t._2 -> 0) // default length increment is 0
  val list: List[(Map[Int, Int], List[Map[Int, Int]])] = semiLinear.toList.map(r => (m0 ++ r._1, r._2.toList.map(t => m0 ++ t)))
  val vecVars: List[(Int, Int)] = List.range(0, list.size).flatMap(i => List.range(0, list(i)._2.size).map(j => (i, j)))

  private def getVarName(x: Int, y: Int): String = "v" + x + "_" + y

  private def getStrName(strV: StrV): String = StrLen(strV).toFormula(nameToIdx)

  private def getIntName(intV: IntV): String = intV.toFormula(nameToIdx)

  private def combine(list: List[String], op: String): String = {
    list.size match {
      case 0 => ""
      case 1 => list(0)
      case _ => list.drop(2).foldLeft("(" + op + " " + list(0) + " " + list(1) + ")") { (x, y) => "(" + op + " " + x + " " + y + ")" }
    }
  }

  def getDeclare: String = {
    val intDef: String = intVs.map(v => "(declare-const " + getIntName(v) + " Int) \n").mkString
    val lenDef: String = strVs.map(v => "(declare-const " + getStrName(v) + " Int)\n").mkString
    val coDef: String = vecVars.map(t => "(declare-const " + getVarName(t._1, t._2) + " Int)\n").mkString
    intDef + lenDef + coDef
  }

  def getLength(): String = {

    def getLinear(x: Int, base: Int, expected: String, m: Map[Int, Int]): String = {
      val value = m.map(y =>
        "(* " + y._2.toString + " " + getVarName(x, y._1) + ")"
      ).foldLeft(base.toString) { (p, q) => "(+ " + p + " " + q + ")" }

      "(= " + expected + " " + value + ")"
    }

    val parikh = combine(List.range(0, list.size).map(i => // i-th linear set
      combine(strVs.toList.filter(x => nameToIdx.contains(x)).map(v =>
        getLinear(i, list(i)._1(nameToIdx(v)), getStrName(v), List.range(0, list(i)._2.size).map(j => j -> list(i)._2(j)(nameToIdx(v))).toMap) //j-th vector
      ), "and")
    ).filter(_.nonEmpty), "or")

    val nonNeg = if (strVs.size + vecVars.size == 0) "" else
      (vecVars.map(t => getVarName(t._1, t._2)) ++ strVs.map(getStrName(_))).map(v => "(<= 0 " + v + ")").foldLeft("true") { (p, q) => "(and " + p + " " + q + ")" }

    val assertParikh = if (parikh.isEmpty) "" else "(assert " + parikh + ")\n"

    val assertNonNeg = if (nonNeg.isEmpty) "" else "(assert " + nonNeg + ")\n"

    assertParikh + assertNonNeg
  }

  def getIntegerCons: String = intCons.map(e => e.toFormula(nameToIdx)).map(s => "(assert " + s + ")\n").mkString


}
