package builder

import java.io.{File, PrintWriter}

import constraint.atomicSL.AtomicSLCons
import constraint.regular.RegCons
import formula.atomic.IntegerEquation
import formula.str.StrV

import scala.sys.process._

case class Checker(source: String, options: Map[String, List[String]]) {

  type Clause = (List[AtomicSLCons], Set[RegCons[Char]], Set[Char], Set[IntegerEquation], Map[StrV, Int])
  val (formula, getModel) = FormulaBuilder(source).output
  val clauses: List[Clause] = SLConsBuilder(formula).output
  val printOption: Boolean = options.contains("-p")

  def output: (Boolean, String) = {
    loopCheck(clauses)
  }

  def loopCheck(clauses: List[Clause]): (Boolean, String) = {
    clauses match {
      case Nil => (false, "")
      case x :: xs => {
        val (sat, witness) = check(x)
        if (sat)
          (sat, witness)
        else
          loopCheck(xs)
      }
    }
  }

  def check(clause: Clause): (Boolean, String) = {
    val (we, sr, chars0, ie, nameToIdx) = clause
    val asciiSize = if (options.contains("-ascii")) options("-ascii").head.toInt else 0
    val ascii = Math.min(256, asciiSize)
    val chars = chars0 ++ List.range(0, ascii).map(_.toChar).toSet
    val split = 655.toChar
    val getLength = ie.flatMap(t => t.strVs).intersect(nameToIdx.keySet).nonEmpty
    val (sstList, sstInt, sstChar, sstSat) = SSTBuilder(we, sr, chars, split, nameToIdx.size, getModel, getLength, printOption).output

    if (!sstSat)
      return (false, "")
    if (ie.isEmpty) {
      if (getModel) {
        val witness = WitnessBuilder("", nameToIdx, sstChar, null, chars, split).output
        return (true, witness)
      }
      else {
        return (true, "")
      }
    }

    val (parikhs, trans) = ParikhBuilder(sstInt, sstList, we).output

    def loopCheckInt(queue : List[Set[(Map[Int, Int], Set[Map[Int, Int]])]]): (Boolean, String) ={
      queue match {
        case Nil => return (false, "")
        case p :: rest=>{
          if (options.contains("-parikh")) {
            println("=========parikh : " + p.size + "===========")
            p.foreach(println)
          }

          val z3Input = Z3InputBuilder(ie.toList, p, nameToIdx, getModel).output
          val z3Output = executeZ3(z3Input)

          if (!z3Output.startsWith("sat"))
            loopCheckInt(rest)
          else {
            if (getModel) {
              val witness = WitnessBuilder(z3Output, nameToIdx, sstChar, trans, chars, split, sstList).output
              (true, witness)
            }
            else
              (true, "")
          }
        }
      }
    }

    loopCheckInt(parikhs.toList)
  }



  def executeZ3(input: String): String = {
    val path = System.getProperty("user.dir") + "\\temp"

    val file = new File(path)

    if (!file.exists()) {
      file.createNewFile()
    }


    val pw = new PrintWriter(file)
    pw.write(input)
    pw.close

    val output: String = try {
      ("z3 -smt2 " + path).!!
    } catch {
      case e: Throwable => {
        e.printStackTrace()
        "unsat"
      }
    }
    //file.delete()
    output
  }
}

