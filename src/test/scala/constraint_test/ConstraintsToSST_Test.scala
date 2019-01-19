package constraint_test

import constraint.SSTBuilder
import constraint.regular.RegCons
import constraint.relational.{Concatenation, RelCons, TransducerConstraint}
import constraint.vars.StringVariable
import deterministic.boundedcopy.SST
import deterministic.examples.{DFAExamples, TransducerExamples}
import org.scalatest.FlatSpec

class ConstraintsToSST_Test extends FlatSpec {

  val dfa1 = DFAExamples.getDFA1

  val trans = TransducerExamples.getHalfTransducer

  val list = List(
    Concatenation(StringVariable(2), List(Left(StringVariable(1)), Left(StringVariable(0)), Right("".toList))),
    TransducerConstraint(StringVariable(3), trans, StringVariable(2)),
    //Concatenation(StringVariable(3), StringVariable(1), StringVariable(2)),
    Concatenation(StringVariable(4), List(Left(StringVariable(1)), Left(StringVariable(0)))),
    Concatenation(StringVariable(5), List(Left(StringVariable(1)), Left(StringVariable(0)))),
  )

  val set = Set[RegCons[Char]](
    RegCons(StringVariable(0), dfa1),
    RegCons(StringVariable(1), dfa1),
  )

  def myAssert(strs: List[String], rels: List[RelCons], regs: Set[RegCons[Char]]): Unit = {
    rels.foreach(r => {
      r match {
        case c: Concatenation[Char] => assert(strs(c.left.id) == c.list.map(x =>
          x match {
            case Left(v) => strs(v.id)
            case Right(l) => l.mkString
          }
        ).mkString)

        case t: TransducerConstraint[Char] => assert(strs(t.left.id) == t.right1.trans(strs(t.right2.id).toList)(t.right1.initialStates))
      }
    })

    regs.foreach(r => {
      assert(r.R.process(strs(r.x.id)))
    })
  }

  "sst builder" should "runs" in {
    val builder = SSTBuilder[Char](Set('a', 'b'), '#')
    val sst = builder.constraintsToSST(list, set)
    SST.print(sst)

    val input = List("ab", "ab")
    val result = sst.process(input.mkString("#") + "#")
    if (result._1)
      myAssert(result._3.mkString.split("#").toList, list, set)
  }
}
