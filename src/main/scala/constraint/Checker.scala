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

}
