package constraint

import java.io.{File, PrintWriter}

import constraint.integer._
import constraint.regular._
import constraint.relational._
import deterministic.boundedcopy.SST
import expression.Z3Exp

import scala.sys.process._

object Checker {

  def process(intCons: IntCons, relCons: List[RelCons], regCons: Set[RegCons[Char]], charSet: Set[Char], split : Char): Boolean = {

    val sst = SSTBuilder(charSet, split).constraintsToSST_Int(relCons, regCons)

    if(sst.f.isEmpty  || intCons==null)
      !sst.f.isEmpty
    else {
      val input = Z3Exp.toZ3Input(intCons, SST.getParikhImage(sst))

      val path = System.getProperty("user.dir") + "\\temp"

      val file = new File(path)

      if (!file.exists())
        file.createNewFile()

      val pw = new PrintWriter(file)
      pw.write(input)
      pw.close

      val output = ("z3 -smt2 " + path).!!

      file.delete()

      !sst.f.isEmpty && output.substring(0, 3) == "sat"
    }
  }

  def process(cons : (String, List[RelCons], Set[RegCons[Char]], Set[Char]), split : Char): Boolean = {
    val (intCons, relCons, regCons, charSet) = cons

    val intEmpty = intCons==null || intCons.isEmpty || intCons.toList.foldLeft(true){(x,y)=> x && y.isWhitespace}

    if(  intEmpty && regCons.isEmpty )
      return true

    println("start construct sst: ")
    val startTime = System.currentTimeMillis()
    val sst = SSTBuilder(charSet, split).constraintsToSST_Int(relCons, regCons)
    val timeCost = System.currentTimeMillis() - startTime
    println("sst is constructed in " + timeCost.toDouble/1000 + "s")
    println("sst states: " + sst.states.size)
    println("sst variables: " + sst.vars.size)
    println("sst transitions: " + sst.δ.size)
    println()


    if(sst.f.isEmpty  || intEmpty )
      !sst.f.isEmpty
    else {

      val input = toZ3Input(intCons, SST.getParikhImage(sst))

      val path = System.getProperty("user.dir") + "\\temp"

      val file = new File(path)

      if (!file.exists())
        file.createNewFile()

      val pw = new PrintWriter(file)
      pw.write(input)
      pw.close

      println("start run z3 smt solver:")
      val startTime = System.currentTimeMillis()
      val output = ("z3 -smt2 " + path).!!
      val timeCost = System.currentTimeMillis() - startTime
      println("z3 solved in " + timeCost.toDouble/1000 + "s\n")

      file.delete()

      !sst.f.isEmpty && output.substring(0, 3) == "sat"
    }
  }

  private def toZ3Input(cons : String, sls: Set[(Map[Int, Int], Set[Map[Int, Int]])]): String = {
    //assert cons is valid
    println("start construct z3 input file:")
    val startTime = System.currentTimeMillis()

    def getStrName(i : Int) = "len"+i

    def getIntName(i : Int) = "x"+i

    def getVarName(x: Int, y: Int): String = "v" + x + "_" + y

    def getLinear(x: Int, base: Int, expected: String, m: Map[Int, Int]): String = {
      val value = m.map(y =>
        "(* " + y._2.toString + " " + getVarName(x, y._1) + ")"
      ).foldLeft(base.toString) { (p, q) => "(+ " + p + " " + q + ")" }

      "(= " + expected + " " + value + ")"
    }

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
      ).foldLeft("true") { (p, q) => "(and " + p + " " +q +")" }
    ).foldLeft("false") { (p, q) => "(or " + p + " " +q +")" }

    val formula = if (cons.isEmpty) formula_0 else "(and " + formula_0 + " " + cons + ")"

    val declare_sentence = declare_0 + "\n" + declare_1 + "\n" + declare_2

    val assert_sentence = "(assert" + formula + ")"

    val end_sentence = "(check-sat)"

    val res = declare_sentence + "\n" + assert_sentence + "\n" + end_sentence

    val timeCost = System.currentTimeMillis() - startTime
    println("z3 input is constructed in " + timeCost.toDouble/1000 + "s")
    println("z3 input length " + res.length)
    println()

    res
  }

}
