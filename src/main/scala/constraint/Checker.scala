package constraint

import java.io.{File, PrintWriter}
import java.util.Calendar

import constraint.regular.RegCons
import constraint.relational.RelCons
import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.SST

import scala.sys.process._

object Checker {

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
    val sst = getSST(charSet, split, relCons, regCons, log)

    if (sst.f.isEmpty || intEmpty)
      !sst.f.isEmpty
    else {
      val pi = getParikhImage(sst, log)
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

  def getSST(charSet: Set[Char], split: Char, relCons: List[RelCons], regCons: Set[RegCons[Char]], log: Boolean) = {
    if (log) {
      println("start constructing sst from constraints: ")
    }
    val startTime = System.currentTimeMillis()
    val res = SSTBuilder(charSet, split).constraintsToSST(relCons, regCons)
    if (log) {
      val timeCost = System.currentTimeMillis() - startTime
      println("states: " + res.states.size)
      println("vars: " + res.vars.size)
      println("transitions: " + res.δ.size)
      println("time cost: " + timeCost.toDouble / 1000 + "s")
      println()
    }
    res
  }

  def getParikhImage(sst: SST[SST_State, Char, Int, SST_Var], log: Boolean) = {
    if (log) {
      println("start constructing Parikh image from sst: ")
    }
    val startTime = System.currentTimeMillis()
    val res = sst.toParikhImage
    if (log) {
      val timeCost = System.currentTimeMillis() - startTime
      println("Parikh image linear sets: " + res.size)
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

  def parseZ3Input(cons: String, sls: Set[(Map[Int, Int], Set[Map[Int, Int]])]): String = {

    def getStrName(i: Int) = "len" + i

    def getIntName(i: Int) = "x" + i

    def getVarName(x: Int, y: Int): String = "v" + x + "_" + y

    def getLinear(x: Int, base: Int, expected: String, m: Map[Int, Int]): String = {
      val value = m.map(y =>
        "(* " + y._2.toString + " " + getVarName(x, y._1) + ")"
      ).foldLeft(base.toString) { (p, q) => "(+ " + p + " " + q + ")" }

      "(= " + expected + " " + value + ")"
    }

    val strVars: Set[Int] = cons.split(" ").toSet.filter(_.startsWith("len")).map(s =>
      if (s.endsWith(")")) s.substring(3, s.length - 1).toInt
      else s.substring(3).toInt)

    val intVars: Set[Int] = cons.split(" ").toSet.filter(_.startsWith("x")).map(s =>
      if (s.endsWith(")")) s.substring(1, s.length - 1).toInt
      else s.substring(1).toInt)

    val list: List[(Map[Int, Int], List[Map[Int, Int]])] = sls.toList.map(r => (r._1, r._2.toList))

    val declare_0 = strVars.map(i => "(declare-const " + getStrName(i) + " Int)").mkString("\n")

    val declare_1 = intVars.map(i => "(declare-const " + getIntName(i) + " Int)").mkString("\n")

    val declare_2 = List.range(0, list.size).flatMap(i => {
      List.range(0, list(i)._2.size).map(j => "(declare-const " + getVarName(i, j) + " Int)")
    }).mkString("\n")

    val formula_0 = List.range(0, list.size).map(i =>
      strVars.map(x =>
        getLinear(i, list(i)._1(x), getStrName(x), List.range(0, list(i)._2.size).map(j => j -> list(i)._2(j)(x)).toMap)
      ).foldLeft("true") { (p, q) => "(and " + p + " " + q + ")" }
    ).foldLeft("false") { (p, q) => "(or " + p + " " + q + ")" }

    val formula = if (cons.isEmpty) formula_0 else "(and " + formula_0 + " " + cons + ")"

    val declare_sentence = declare_0 + "\n" + declare_1 + "\n" + declare_2

    val assert_sentence = "(assert" + formula + ")"

    val end_sentence = "(check-sat)"

    declare_sentence + "\n" + assert_sentence + "\n" + end_sentence
  }

  def executeZ3(input: String): String = {

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
