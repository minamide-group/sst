package expression

import constraint.integer._
import constraint.integer.term._

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

  def toZ3Input(cons : IntCons, sls: Set[(Map[Int, Int], Set[Map[Int, Int]])]): String ={

    def getStringVars(cons : IntCons): Set[Int] ={
      def getFromTerm(t : Term) : Set[Int] = {
        t match {
          case a : Length => Set(a.x.id)
          case a : Add => getFromTerm(a.t1) ++ getFromTerm(a.t2)
          case a : Sub => getFromTerm(a.t1) ++ getFromTerm(a.t2)
          case _ => Set()
        }
      }
      cons match {
        case a : IntAnd => getStringVars(a.p) ++ getStringVars(a.q)
        case a : IntOr => getStringVars(a.p) ++ getStringVars(a.q)
        case a : IntNot => getStringVars(a.p)
        case e : IntEqual => getFromTerm(e.a) ++ getFromTerm(e.t)
        case e : IntGT => getFromTerm(e.a) ++ getFromTerm(e.t)
        case e : IntGTE => getFromTerm(e.a) ++ getFromTerm(e.t)
        case e : IntLT => getFromTerm(e.a) ++ getFromTerm(e.t)
        case e : IntLTE => getFromTerm(e.a) ++ getFromTerm(e.t)
      }
    }

    def getIntVars(cons : IntCons) : Set[Int] = {
      def getFromTerm(t : Term) : Set[Int] = {
        t match {
          case a : IntVar => Set(a.i)
          case a : Add => getFromTerm(a.t1) ++ getFromTerm(a.t2)
          case a : Sub => getFromTerm(a.t1) ++ getFromTerm(a.t2)
          case _ => Set()
        }
      }
      cons match {
        case a : IntAnd => getIntVars(a.p) ++ getIntVars(a.q)
        case a : IntOr => getIntVars(a.p) ++ getIntVars(a.q)
        case a : IntNot => getIntVars(a.p)
        case e : IntEqual => getFromTerm(e.a) ++ getFromTerm(e.t)
        case e : IntGT => getFromTerm(e.a) ++ getFromTerm(e.t)
        case e : IntGTE => getFromTerm(e.a) ++ getFromTerm(e.t)
        case e : IntLT => getFromTerm(e.a) ++ getFromTerm(e.t)
        case e : IntLTE => getFromTerm(e.a) ++ getFromTerm(e.t)
      }
    }

    def getStrName(i : Int) = "len"+i

    def getIntName(i : Int) = "x"+i

    def getVarName(x: Int, y: Int): String = "v" + x + "_" + y

    def getLinear(x: Int, base: Int, expected: String, m: Map[Int, Int]): Exp = Op2Exp("=", StringExp(expected),
      m.map(y => Op2Exp("*", StringExp(y._2.toString), StringExp(getVarName(x, y._1)))).foldLeft(StringExp(base.toString): Exp) { (p, q) => Op2Exp("+", p, q) })

    def consToFormula(cons : IntCons) : Exp ={
      def termToFormula(t : Term) : Exp = {
        t match {
          case a : Add => Op2Exp("+", termToFormula(a.t1), termToFormula(a.t2))
          case a : Sub => Op2Exp("-", termToFormula(a.t1), termToFormula(a.t2))
          case a : IntConst => StringExp(a.c.toString)
          case a : IntVar => StringExp(getIntName(a.i))
          case a : Length => StringExp(getStrName(a.x.id))
        }
      }

      cons match {
        case a : IntAnd => Op2Exp("and", consToFormula(a.p), consToFormula(a.q))
        case a : IntOr => Op2Exp("or", consToFormula(a.p), consToFormula(a.q))
        case a : IntNot => Op1Exp("not", consToFormula(a.p))
        case e : IntEqual => Op2Exp("=", termToFormula(e.a), termToFormula(e.t))
        case e : IntGT => Op2Exp(">", termToFormula(e.a), termToFormula(e.t))
        case e : IntGTE => Op2Exp(">=", termToFormula(e.a), termToFormula(e.t))
        case e : IntLT => Op2Exp("<", termToFormula(e.a), termToFormula(e.t))
        case e : IntLTE => Op2Exp("<=", termToFormula(e.a), termToFormula(e.t))
      }
    }

    val strVars : Set[Int] = getStringVars(cons)

    val intVars : Set[Int] = getIntVars(cons)

    val list : List[(Map[Int,Int], List[Map[Int,Int]])] = sls.toList.map(r=> (r._1, r._2.toList))

    val declare_0 = strVars.map(i => "(declare-const " + getStrName(i) + " Int)").mkString("\n")

    val declare_1 = intVars.map(i => "(declare-const " + getIntName(i) + " Int)").mkString("\n")

    val declare_2 = List.range(0, list.size).flatMap(i=>{
      List.range(0, list(i)._2.size).map(j => "(declare-const " + getVarName(i, j) + " Int)")
    }).mkString("\n")

    val formula_0 = List.range(0, list.size).map(i=>
      strVars.map(x=>
        getLinear(i, list(i)._1(x), getStrName(x), List.range(0, list(i)._2.size).map(j=> j-> list(i)._2(j)(x)).toMap )
      ).foldLeft(StringExp("true"): Exp) { (p, q) => Op2Exp("and", p, q) }
    ).foldLeft(StringExp("false"): Exp) { (p, q) => Op2Exp("or", p, q) }

    val formula_1 = consToFormula(cons)

    val formula = Op2Exp("and", formula_0, formula_1)

    val declare_sentence = declare_0 + "\n" + declare_1 + "\n" + declare_2

    val assert_sentence = "(assert" + eval(formula) + ")"

    val end_sentence = "(check-sat)"

    declare_sentence + "\n" + assert_sentence + "\n" + end_sentence
  }

  def toZ3Input(cons : String, sls: Set[(Map[Int, Int], Set[Map[Int, Int]])]): String = {
    //assert cons is valid

    def getStrName(i : Int) = "len"+i

    def getIntName(i : Int) = "x"+i

    def getVarName(x: Int, y: Int): String = "v" + x + "_" + y

    def getLinear(x: Int, base: Int, expected: String, m: Map[Int, Int]): Exp = Op2Exp("=", StringExp(expected),
      m.map(y => Op2Exp("*", StringExp(y._2.toString), StringExp(getVarName(x, y._1)))).foldLeft(StringExp(base.toString): Exp) { (p, q) => Op2Exp("+", p, q) })

    val strVars : Set[Int] = cons.split(" ").toSet.filter(_.startsWith("len")).map(s=>
      if(s.endsWith(")")) s.substring(3, s.length-1).toInt
      else s.substring(3).toInt)

    val intVars : Set[Int] = cons.split(" ").toSet.filter(_.startsWith("x")).map(s=>
      if(s.endsWith(")")) s.substring(1, s.length-1).toInt
      else s.substring(1).toInt)

    val list : List[(Map[Int,Int], List[Map[Int,Int]])] = sls.toList.map(r=> (r._1, r._2.toList))

    val declare_0 = strVars.map(i => "(declare-const " + getStrName(i) + " Int)").mkString("\n")

    val declare_1 = intVars.map(i => "(declare-const " + getIntName(i) + " Int)").mkString("\n")

    val declare_2 = List.range(0, list.size).flatMap(i=>{
      List.range(0, list(i)._2.size).map(j => "(declare-const " + getVarName(i, j) + " Int)")
    }).mkString("\n")

    val formula_0 = List.range(0, list.size).map(i=>
      strVars.map(x=>
        getLinear(i, list(i)._1(x), getStrName(x), List.range(0, list(i)._2.size).map(j=> j-> list(i)._2(j)(x)).toMap )
      ).foldLeft(StringExp("true"): Exp) { (p, q) => Op2Exp("and", p, q) }
    ).foldLeft(StringExp("false"): Exp) { (p, q) => Op2Exp("or", p, q) }

    val formula = if (cons.isEmpty) formula_0 else Op2Exp("and", formula_0, StringExp(cons))

    val declare_sentence = declare_0 + "\n" + declare_1 + "\n" + declare_2

    val assert_sentence = "(assert" + eval(formula) + ")"

    val end_sentence = "(check-sat)"

    declare_sentence + "\n" + assert_sentence + "\n" + end_sentence
  }
}
