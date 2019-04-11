package constraint

import java.io.{File, PrintWriter}
import java.util.Calendar

import constraint.regular.RegCons
import constraint.relational.{Concatenation, RelCons}
import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.SST

import scala.sys.process._

object Checker {

  type MySST[X] = SST[SST_State, Char, X, SST_Var]

  def process(chars: String, rl: List[String], rg: List[String], ic: String, split: Char, log: Boolean): Boolean = {
    if (log) {
      println("------start check at " + Calendar.getInstance.getTime + "---------")
      val startTime = System.currentTimeMillis()
      try {
        val res = processInner(chars, rl, rg, ic, split, log)
        val timeCost = System.currentTimeMillis() - startTime
        println("result: " + (if (res) "sat" else "unsat"))
        println("total time cost : " + timeCost.toDouble / 1000 + "s")
        println("--------end check at " + Calendar.getInstance.getTime + "---------")
        println()
        println()
        res
      } catch {
        case e: Throwable => {
          println(e.getMessage)
          println(e.getCause)
          e.printStackTrace()
          println("------------break at " + Calendar.getInstance.getTime + "---------")
          false
        }
      }
    }
    else
      processInner(chars, rl, rg, ic, split, log)
  }

  private def processInner(chars: String, rl: List[String], rg: List[String], ic: String, split: Char, log: Boolean): Boolean = {
    val (intCons, relCons, regCons, charSet) = getConstraints(chars, rl, rg, ic, split, log)
    val intEmpty = intCons == null || intCons.isEmpty || intCons.toList.foldLeft(true) { (x, y) => x && y.isWhitespace }
    if (intEmpty && regCons.isEmpty)
      return true

    val sstList = getSSTList(charSet, split, relCons, regCons, log)
    val sst_option = getComposedSST(charSet, split, sstList, log)
    if (sst_option.isEmpty || intEmpty)
      sst_option.nonEmpty
    else {
      val sst = sst_option.get

      val pi = getParikhImage(sst, sstList, relCons, log)
      val input = getZ3Input(intCons, pi, log)
      val output = getZ3Output(input, log)
      !sst.f.isEmpty && output.substring(0, 3) == "sat"
    }
  }

  def getConstraints(chars: String, rl: List[String], rg: List[String], ic: String, split: Char, log: Boolean) = {
    if (log) {
      println("start constructing constraints from input String: ")
    }
    val startTime = System.currentTimeMillis()
    val res = ConstraintBuilder(chars, rl, rg, ic).toConstraints
    if (log) {
      val timeCost = System.currentTimeMillis() - startTime
      println("alphabets: " + res._4.size)
      println("relational: " + res._2.size)
      println("regular: " + res._3.size)
      println("time cost: " + timeCost.toDouble / 1000 + "s")
      println()
    }
    res
  }

  def getComposedSST(charSet: Set[Char], split: Char, sstList: List[MySST[Char]], log: Boolean) = {
    if (log) {
      println("start composing ssts: ")
    }
    val startTime = System.currentTimeMillis()
    val res = SSTBuilder(charSet, split).composeSSTs(sstList)
    if (log) {
      val
      timeCost = System.currentTimeMillis() - startTime
      if (res.nonEmpty)
        res.get.print
      println("time cost: " + timeCost.toDouble / 1000 + "s")
      println()
    }
    res
  }

  def getSSTList(charSet: Set[Char], split: Char, relCons: List[RelCons], regCons: Set[RegCons[Char]], log: Boolean) = {
    if (log) {
      println("start constructing sst from constraints: ")
    }
    val startTime = System.currentTimeMillis()
    val res = SSTBuilder(charSet, split).constraintsToSSTs(relCons, regCons)
    if (log) {
      val timeCost = System.currentTimeMillis() - startTime
      println("sst num: " + res.size)
      res.foreach(_.print)
      println("time cost: " + timeCost.toDouble / 1000 + "s")
      println()
    }
    res
  }

  def getParikhImage(sst: MySST[Int], sstList: List[MySST[Char]], relCons: List[RelCons], log: Boolean) = {
    def f(idx: Int, idxLength: Map[Int, Int], charNum: Int, idxNum: Map[Int, Int]) = {
      idxLength + (idx -> (charNum + idxNum.map(t => t._2 * idxLength(t._1)).reduce(_ + _)))
    }

    if (log) {
      println("start constructing Parikh image from sst: ")
    }
    val startTime = System.currentTimeMillis()
    val res0 = sst.toParikhImage
    val res = relCons.last match {
      case c: Concatenation[Char] if sstList.size == relCons.size => {
        val charNum = c.list.collect { case Right(l) => l.size }.reduce(_ + _)
        val varsIdxNum = c.list.collect { case Left(v) => v.id }.groupBy(identity).map(t => t._1 -> t._2.size)
        res0.map(linearSet => (f(c.left.id, linearSet._1, charNum, varsIdxNum), linearSet._2.map(vec => f(c.left.id, vec, 0, varsIdxNum))))
      }
      case _ => res0
    }

    if (log) {
      val timeCost = System.currentTimeMillis() - startTime
      println("Parikh image linear sets: " + res.size)
      res.foreach(println)
      println("time cost: " + timeCost.toDouble / 1000 + "s")
      println()
    }
    res
  }

  def getZ3Input(cons: String, sls: Set[(Map[Int, Int], Set[Map[Int, Int]])], log: Boolean) = {
    if (log) {
      println("start constructing Z3 input from Parikh image and integer constraints: ")
    }
    val startTime = System.currentTimeMillis()
    val res = parseZ3Input(cons, sls)
    if (log) {
      val timeCost = System.currentTimeMillis() - startTime
      println("z3 input file length: " + res.length)
      println("time cost: " + timeCost.toDouble / 1000 + "s")
      println()
    }
    res
  }

  def getZ3Output(input: String, log: Boolean): String = {
    if (log) {
      println("start running z3 smt solver: ")
    }
    val startTime = System.currentTimeMillis()
    val res = executeZ3(input)
    if (log) {
      val timeCost = System.currentTimeMillis() - startTime
      println("z3 output: " + res)
      println("time cost: " + timeCost.toDouble / 1000 + "s")
      println()
    }
    res.substring(0, 3)
  }

  private def parseZ3Input(cons: String, sls: Set[(Map[Int, Int], Set[Map[Int, Int]])]): String = {

    def getStrName(i: Int) = "len" + i

    def getIntName(i: Int) = "x" + i

    def getVarName(x: Int, y: Int): String = "v" + x + "_" + y

    def getLinear(x: Int, base: Int, expected: String, m: Map[Int, Int]): String = {
      val value = m.map(y =>
        "(* " + y._2.toString + " " + getVarName(x, y._1) + ")"
      ).foldLeft(base.toString) { (p, q) => "(+ " + p + " " + q + ")" }

      "(= " + expected + " " + value + ")"
    }

    val strVars: Set[Int] = cons.replaceAll("\t", " ").split(" ").toSet.filter(_.startsWith("len")).map(
      s => s.filterNot(_ == ')').substring(3).toInt)

    val m0 : Map[Int, Int] = strVars.map(i=>i->0).toMap // default map

    val intVars: Set[Int] = cons.replaceAll("\t", " ").split(" ").toSet.filter(_.startsWith("x")).map(
      s => s.filterNot(_ == ')').substring(1).toInt)

    val list: List[(Map[Int, Int], List[Map[Int, Int]])] = sls.toList.map(r => ( m0 ++ r._1, r._2.toList.map(t=> m0 ++ t) ))

    val declare_0 = strVars.map(i => "(declare-const " + getStrName(i) + " Int)").mkString("\n")

    val declare_1 = intVars.map(i => "(declare-const " + getIntName(i) + " Int)").mkString("\n")

    val vecVars: List[(Int, Int)] = List.range(0, list.size).flatMap(i => {
      List.range(0, list(i)._2.size).map(j => (i, j))
    })

    val declare_2 = vecVars.map(t => "(declare-const " + getVarName(t._1, t._2) + " Int)").mkString("\n")

    val parikh = if (list.isEmpty) "" else
      List.range(0, list.size).map(i =>
        strVars.map(x =>
          getLinear(i, list(i)._1(x), getStrName(x), List.range(0, list(i)._2.size).map(j => j -> list(i)._2(j)(x)).toMap)
        ).foldLeft("true") { (p, q) => "(and " + p + " " + q + ")" }
      ).foldLeft("false") { (p, q) => "(or " + p + " " + q + ")" }

    val posVar = if (strVars.size + vecVars.size == 0) "" else
      (vecVars.map(t => getVarName(t._1, t._2)) ++ strVars.map(getStrName(_))).map(v => "(<= 0 " + v + ")").foldLeft("true") { (p, q) => "(and " + p + " " + q + ")" }

    val formulaList = List(posVar, parikh, cons).filter(_.nonEmpty)

    val formula = formulaList.size match {
      case 0 => "true"
      case 1 => formulaList(0)
      case 2 => "(and " + formulaList(0) + " " + formulaList(1) + ")"
      case 3 => "(and (and " + formulaList(0) + " " + formulaList(1) + ") " + formulaList(2) + ")"
    }

    val declare_sentence = declare_0 + "\n" + declare_1 + "\n" + declare_2

    val assert_sentence = "(assert" + formula + ")"

    val end_sentence = "(check-sat)"

    declare_sentence + "\n" + assert_sentence + "\n" + end_sentence
  }

  private def executeZ3(input: String): String = {

    val path = System.getProperty("user.dir") + "\\temp"

    val file = new File(path)

    if (!file.exists())
      file.createNewFile()

    val pw = new PrintWriter(file)
    pw.write(input)
    pw.close

    val output = ("z3 -smt2 " + path).!!

    file.delete()

    output
  }
}
