package regex

object Z3Exp {

  private trait Exp

  private case class StringExp(str: String) extends Exp

  private case class Op2Exp(o: String, e1: Exp, e2: Exp) extends Exp

  private case class Op1Exp(o: String, e: Exp) extends Exp

  private def eval(e: Exp): String = {
    e match {
      case s: StringExp => s.str
      case s: Op2Exp => "(" + s.o + " " + eval(s.e1) + " " + eval(s.e2) + ")"
      case s: Op1Exp => "(" + s.o + " " + eval(s.e) + ")"
    }
  }

  private def getVarName(x: Int, y: Int): String = "v" + x + "_" + y

  private def getLinear(x: Int, base: Int, expected: Int, m: Map[Int, Int]): Exp = Op2Exp("=", StringExp(expected.toString),
    m.map(y => Op2Exp("*", StringExp(y._2.toString), StringExp(getVarName(x, y._1)))).foldLeft(StringExp(base.toString): Exp) { (p, q) => Op2Exp("+", p, q) })

  def toZ3Input[T](m: Map[T, Int], sls: Set[(Map[T, Int], Set[Map[T, Int]])]): String = {
    val idxMap = (sls.toList.indices zip sls).map(x => x._1 -> (x._2._1, (x._2._2.toList.indices zip x._2._2).toMap))
    val charSet: Set[T] = sls.flatMap(x => x._1.map(y => y._1)) ++ m.keys
    val formula = idxMap.map(
      x => charSet.map(c =>
        getLinear(x._1, x._2._1.withDefaultValue(0)(c), m.withDefaultValue(0)(c), x._2._2.map(y => y._1 -> y._2.withDefaultValue(0)(c)))
      ).foldLeft(StringExp("true"): Exp) { (p, q) => Op2Exp("and", p, q) }
    ).foldLeft(StringExp("false"): Exp) { (p, q) => Op2Exp("or", p, q) }

    val declare_sentence = idxMap.flatMap(x => x._2._2.map(y => getVarName(x._1, y._1))).map(q => "(declare-const " + q + " Int)").mkString("\n")
    val assert_sentence = "(assert" + eval(formula) + ")"
    val end_sentence = "(check-sat)"

    declare_sentence + "\n" + assert_sentence + "\n" + end_sentence
  }

}
