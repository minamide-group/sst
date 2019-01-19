package constraint_test

import constraint.SSTBuilder
import constraint.integer._
import constraint.integer.term._
import constraint.regular.RegCons
import constraint.relational._
import constraint.vars._
import deterministic.boundedcopy.SST
import deterministic.examples._
import org.scalatest.FlatSpec
import expression.Z3Exp

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

  val intCons = IntAnd(
    IntEqual(Length(StringVariable(4)), IntConst(5)),
    IntEqual(Length(StringVariable(5)), IntConst(5))
  )


  "sst builder" should "runs" in {
    val builder = SSTBuilder[Char](Set('a', 'b'), '#')
    val sst = builder.constraintsToSST_Int(relCons, regCons)
    SST.print(sst)

    val pi = SST.getParikhImage(sst)
    pi.foreach(println)

    val z3 = Z3Exp.toZ3Input(intCons, pi)

    println(z3)
  }
}
