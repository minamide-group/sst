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
    if(formula==null){
      return (Some(true), List(("No constraint","")))
    }

    val t2 = System.currentTimeMillis()

    val (clauses, msg_SL) = SLConsBuilder(formula).output
    val t3 = System.currentTimeMillis()

    val message = msg_SL:::List(
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
      val name = "Clause number"
      val value = clauses.size.toString
      val (res, msg) = checkLoop(clauses, message:::List((name, value)))
      (res, msg)
    }
  }

  def checkLoop(clauses : List[(List[AtomicSLCons], Set[RegCons[Char]], Set[Char], Set[IntegerEquation], Map[StrV, Int])],
                msg : List[(String, String)]): (Option[Boolean], List[(String, String)]) ={
    clauses match {
      case Nil =>(Some(false), msg)
      case x :: xs =>{

        val (we, sr, chars, ie, map) = x

        if(we.size ==0 && sr.size==0 && ie.size==0) {
          val message = msg ::: List(("Empty clause",""))
          return (Some(true), message)
        }

        if(we.size ==0 && sr.size==0){
          val z3Input = Z3InputBuilder(ie.toList, Set(), map).output
          val message = msg ::: List(("Only contains integer constraints",""))
          val (z3sat, newMsg) = excuteZ3(z3Input, message)
          if(z3sat)
            return (Some(true), newMsg)
          else
            return checkLoop(xs, newMsg)
        }

        val sstOption = SSTBuilder(we, sr, chars, 0.toChar, map.size).output

        val message = (msg ::: List( ("chars", chars.toString))) :::sstOption._3
        if(sstOption._2.isEmpty || sstOption._2.get.states.isEmpty){
          val name = "Clause Unsatisfiable"
          val value = ""
          return checkLoop(xs, message ::: List((name, value)))
        }

        else if(ie.isEmpty){
          val name = "Satisfiable"
          val value = "clause sat, no integer constraint"
          return (Some(true), message ::: List((name, value)) )
        }

        val sst = sstOption._2.get
        //sst.printDetail
        val sstList = sstOption._1.get
        //sstList.foreach(i=>i.printDetail)
        val parikh = ParikhBuilder(sst, sstList, we).output
        val z3Input = Z3InputBuilder(ie.toList, parikh, map).output
        //val msg_Z3 = message:::List(("Z3 input", z3Input))
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
      (true, msg ::: List(("Satisfiable","Z3 sat"), ("Z3 check time", getTimeSecond(t1,t2))) )
    else
      (false, msg ::: List(("Unsatisfiable", "Z3 unsat"), ("Z3 check time", getTimeSecond(t1,t2))) )
  }
}
