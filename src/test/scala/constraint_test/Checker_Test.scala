package constraint_test

import constraint.Checker
import constraint.integer._
import constraint.integer.term._
import constraint.regular.RegCons
import constraint.relational.Concatenation
import constraint.vars.StringVariable
import deterministic.examples.DFAExamples
import org.scalatest.FlatSpec

class Checker_Test extends FlatSpec {

  val relCons = List(
    Concatenation(StringVariable(2), List(Left(StringVariable(1)), Left(StringVariable(0)))),
    //TransducerConstraint(StringVariable(3), TransducerExamples.getHalfTransducer, StringVariable(2)),
    Concatenation(StringVariable(3), List(Left(StringVariable(1)), Left(StringVariable(2)))),
    //Concatenation(StringVariable(4), StringVariable(1), StringVariable(2)),
  )

  val regCons = Set[RegCons[Char]](
    RegCons(StringVariable(0), DFAExamples.getDFA1),
  )

  val intCons = IntAnd(
    IntEqual(Length(StringVariable(3)), IntConst(5)),
    IntNot(
      IntGT(Length(StringVariable(2)), Length(StringVariable(3)))
    )
  )

  "process" should "run" in {
    val res = Checker.process(intCons, relCons, regCons, Set('a', 'b'), '#')
    assert(res)
  }
}
