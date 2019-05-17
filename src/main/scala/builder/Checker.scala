package builder

import java.io.{File, PrintWriter}

import constraint.atomicSL.AtomicSLCons
import constraint.regular.RegCons
import formula.atomic.IntegerEquation
import formula.str.StrV

import scala.io.Source
import scala.sys.process._

case class Checker(file : File, asciiSize : Int) {

  type Clause = (List[AtomicSLCons], Set[RegCons[Char]], Set[Char], Set[IntegerEquation], Map[StrV, Int])
  val lines = Source.fromFile(file.getPath).getLines().toList
  val (formula, getModel) = FormulaBuilder(lines).output
  val clauses : List[Clause] = SLConsBuilder(formula).output

  def output: (Boolean, String) = {
    loop(clauses)
  }

  def loop(clauses: List[Clause])  : (Boolean, String) = {
    clauses match {
      case Nil => (false, "")
      case x :: xs => {
        val (sat, witness) = check(x)
        if(sat)
          (sat, witness)
        else
          loop(xs)
      }
    }
  }

  def check(clause: Clause) : (Boolean, String) = {
    val (we, sr, chars0, ie, nameToIdx) = clause
    val ascii = Math.min(256, asciiSize)
    val chars = chars0 ++ List.range(0, ascii).map(_.toChar).toSet

    val split = 655.toChar
    val getLength = ie.flatMap(t=>t.strVs).intersect(nameToIdx.keySet).nonEmpty
    val (sstList, sstInt, sstChar, sstSat) = SSTBuilder(we, sr, chars, split, nameToIdx.size, getModel, getLength).output

    if(!sstSat)
      return (false, "")
    if(ie.isEmpty){
      if(getModel) {
        val witness = WitnessBuilder("", nameToIdx, sstChar, null, chars, split).output
        return (true, witness)
      }
      else{
        return (true, "")
      }
    }

    val (semiLinear, trans) = ParikhBuilder(sstInt, sstList, we).output
    val z3Input = Z3InputBuilder(ie.toList, semiLinear, nameToIdx, getModel).output
    val z3Output = executeZ3(z3Input)

    if(!z3Output.startsWith("sat")) {
      return (false, "")
    }
    else{
      if(getModel) {
        val witness = WitnessBuilder(z3Output, nameToIdx, sstChar, trans, chars, split).output
        return (true, witness)
      }
      else
        return  (true, "")
    }
  }

  def executeZ3(input : String): String = {
    val path = System.getProperty("user.dir") + "\\temp"

    val file = new File(path)

    if (!file.exists())
      file.createNewFile()

    val pw = new PrintWriter(file)
    pw.write(input)
    pw.close

    val output : String = try{
      ("z3 -smt2 " + path).!!
    }catch {
      case _ : Throwable => "unsat"
    }
    file.delete()

    output
  }
}
