package constraint_test

import constraint.SSTBuilder
import constraint.regular.RegCons
import constraint.relational.{Concatenation, RelCons, TransducerConstraint}
import constraint.vars.{StringVariable}
import deterministic.boundedcopy.SST
import deterministic.factories.{DFAFactory, TransducerFactory}
import org.scalatest.FlatSpec

class ConstraintsToSST_Test extends FlatSpec {

  val dfa1 = DFAFactory.getDFA0

  val trans = TransducerFactory.getHalfTransducer

  val list = List(
    Concatenation(StringVariable(2), StringVariable(1), StringVariable(0)),
    TransducerConstraint(StringVariable(3), trans, StringVariable(2)),
    //Concatenation(StringVariable(3), StringVariable(1), StringVariable(2)),
    Concatenation(StringVariable(4), StringVariable(1), StringVariable(2)),
    Concatenation(StringVariable(5), StringVariable(1), StringVariable(2)),
  )

  val set = Set[RegCons[Char]](
    RegCons(StringVariable(0), dfa1)
  )

  def myAssert(strs: List[String], rels: List[RelCons], regs: Set[RegCons[Char]]): Unit = {
    rels.foreach(r => {
      r match {
        case c: Concatenation => assert(strs(c.left.id) == strs(c.right1.id) + strs(c.right2.id))
        case t: TransducerConstraint[Char] => assert(strs(t.left.id) == t.right1.trans(strs(t.right2.id).toList)(t.right1.initialStates))
      }
    })

    regs.foreach(r => {
      assert(r.R.process(strs(r.x.id))._1)
    })
  }

  "sst builder" should "runs" in {
    val builder = SSTBuilder[Char](Set('a', 'b'), '#')
    val sst = builder.constraintsToSST(list, set)
    SST.print(sst)

    val input = List("ab", "bba")
    val result = sst.process(input.mkString("#") + "#")
    if (result._1)
      myAssert(result._3.mkString.split("#").toList, list, set)
  }
}
