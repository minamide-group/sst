package builder

import java.io.{File, PrintWriter}

import constraint.atomicSL.AtomicSLCons
import constraint.regular.RegCons
import formula.atomic.IntegerEquation
import formula.str.StrV

import scala.io.Source
import scala.sys.process._

case class Checker(file : File) {

  def getTimeSecond(start : Long, end : Long) : String = ((end-start)*1.0/1000).toString

  def output: (Option[Boolean], List[(String, String)]) ={
    val lines = Source.fromFile(file.getPath).getLines().toList
    val t1 = System.currentTimeMillis()

    val formula = FormulaBuilder(lines).output

    val t2 = System.currentTimeMillis()

    val clauses = SLConsBuilder(formula).output
    val t3 = System.currentTimeMillis()

    val message = List(
      ("Build formula time",  getTimeSecond(t1,t2)),
      ("Build sl time", getTimeSecond(t2,t3))
    )

    if(clauses.isEmpty){
      val name = "Building Formula Failed"
      val value = "No Straight Line Clauses"
      (None, message:::List((name, value)))
    }
    else{
      //foreach clause, check sat
      val name = "clause number"
      val value = clauses.size.toString
      val (res, msg) = checkLoop(clauses, message:::List((name, value)))
      val t4 = System.currentTimeMillis()
      (res, msg ::: List(("Total time",getTimeSecond(t3,t4))))
    }
  }

  def checkLoop(clauses : List[(List[AtomicSLCons], Set[RegCons[Char]], Set[Char], Set[IntegerEquation], Map[StrV, Int])],
                msg : List[(String, String)]): (Option[Boolean], List[(String, String)]) ={
    clauses match {
      case Nil =>{
        val name = "Unsatisfiable"
        val value = "No SL clause is satisfiable"
        (Some(false), msg ::: List((name, value)))
      }
      case x :: xs =>{
        val t1 = System.currentTimeMillis()
        val (we, sr, chars, ie, map) = x
        val sstOption = SSTBuilder(we, sr, chars, 0.toChar).output
        val t2 = System.currentTimeMillis()

        val message = msg ::: List(
          ("SST compose time", getTimeSecond(t1,t2))
        )
        if(sstOption._2.isEmpty || sstOption._2.get.states.isEmpty){
          val name = "Unsatisfiable"
          val value = "clause unsat"
          return checkLoop(xs, message ::: List((name, value)))
        }

        else if(ie.isEmpty){
          val name = "Satisfiable"
          val value = "clause sat, no integer constraint"
          return (Some(true), message ::: List((name, value)) )
        }

        val sst = sstOption._2.get
        val sstList = sstOption._1.get
        val parikh = ParikhBuilder(sst, sstList, we).output
        val z3Input = Z3InputBuilder(ie.toList, parikh, map).output

        val (z3sat, newMsg) = excuteZ3(z3Input, message)
        if(z3sat)
          return (Some(true), newMsg)
        else
          return checkLoop(xs, newMsg)
      }
    }
  }

  def excuteZ3(input : String, msg : List[(String, String)]) : (Boolean, List[(String, String)]) ={
    val path = System.getProperty("user.dir") + "\\temp"

    val file = new File(path)

    if (!file.exists())
      file.createNewFile()

    val pw = new PrintWriter(file)
    pw.write(input)
    pw.close

    val t1 = System.currentTimeMillis()
    val output = ("z3 -smt2 " + path).!!
    val t2 = System.currentTimeMillis()

    file.delete()

    val sat = output.substring(0, 3) == "sat"
    if(sat)
      (true, msg ::: List(("Satisfiable","clause sat, with integer constraints"), ("Z3 check time", getTimeSecond(t1,t2))) )
    else
      (false, msg ::: List(("Unsatisfiable", "clause unsat, integer constraints unsat"), ("Z3 check time", getTimeSecond(t1,t2))) )
  }
}
