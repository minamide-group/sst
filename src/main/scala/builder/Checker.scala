package builder

import java.io.{File, PrintWriter}

import constraint.atomicSL.AtomicSLCons
import constraint.regular.RegCons
import formula.atomic.IntegerEquation
import formula.str.StrV

import scala.io.Source
import scala.sys.process._

case class Checker(file : File) {

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
    val (we, sr, chars, ie, nameToidx) = clause
    val split = 0.toChar
    val (sstList, sstInt, sstChar) = SSTBuilder(we, sr, chars, split, nameToidx.size, getModel).output

    if(sstList.isEmpty || sstInt.isEmpty || sstInt.get.states.isEmpty)
      return (false, "")
    if(ie.isEmpty){
      if(getModel) {
        val witness = WitnessBuilder("", nameToidx, sstChar.get, null, chars, split).output
        return (true, witness)
      }
      else{
        return (true, "")
      }
    }


    val semilinearSet = ParikhBuilder(sstInt.get, sstList.get, we).output
    val z3Input = Z3InputBuilder(ie.toList, semilinearSet, nameToidx, getModel).output
    val z3Output = executeZ3(z3Input)


    if(!z3Output.startsWith("sat")) {
      return (false, "")
    }
    else{
      if(getModel) {
        val witness = WitnessBuilder(z3Output, nameToidx, sstChar.get, sstInt.get, chars, split).output
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

    val output = ("z3 -smt2 " + path).!!

    file.delete()

    output
  }
}
