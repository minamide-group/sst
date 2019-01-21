package constraint_test

import constraint.{Checker, SSTBuilder}
import constraint.regular.RegCons
import constraint.relational._
import constraint.vars._
import deterministic.boundedcopy.SST
import deterministic.examples._
import org.scalatest.FlatSpec

class ConstraintsToSST_Int_Test extends FlatSpec {

  val relCons = List(
    Concatenation(StringVariable(2), List(Left(StringVariable(1)), Left(StringVariable(0))) ),
    //TransducerConstraint(StringVariable(3), TransducerExamples.getHalfTransducer, StringVariable(2)),
    Concatenation(StringVariable(3), List(Left(StringVariable(1)), Left(StringVariable(2))) ),
    Concatenation(StringVariable(4), List(Left(StringVariable(1)), Left(StringVariable(2))) ),
    Concatenation(StringVariable(5), List(Left(StringVariable(1)), Left(StringVariable(2))) ),
  )

  val regCons = Set[RegCons[Char]](
    RegCons(StringVariable(0), DFAExamples.getDFA1),
  )

  val intCons = "(and (= len4 5)  (= len5 5) )"


  "sst builder" should "runs" in {
    val builder = SSTBuilder[Char](Set('a', 'b'), '#')
    val sst = builder.constraintsToSST(relCons, regCons)
    sst.print

    val pi = sst.toParikhImage
    pi.foreach(println)

    val z3 = Checker.getZ3Input(intCons, pi, false)

    println(z3)
  }
}
