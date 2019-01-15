package constraint_test

import constraint.SSTBuilder
import constraint.integer.term._
import constraint.integer._
import constraint.regular.RegCons
import constraint.relational._
import constraint.vars._
import deterministic.boundedcopy.SST
import deterministic.factories._
import org.scalatest.FlatSpec
import regex.Z3Exp

class ConstraintsToSST_Int_Test  extends FlatSpec {

  val dfa1 = DFAFactory.getDFA0

  val trans = TransducerFactory.getHalfTransducer

  val relCons = List(
    Concatenation(StringVariable(2), StringVariable(1), StringVariable(0)),
    TransducerConstraint(StringVariable(3), trans, StringVariable(2)),
    //Concatenation(StringVariable(3), StringVariable(1), StringVariable(2)),
    Concatenation(StringVariable(4), StringVariable(1), StringVariable(2)),
    Concatenation(StringVariable(5), StringVariable(1), StringVariable(2)),
  )

  val regCons = Set[RegCons[Char]](
    //RegCons(StringVariable(0), dfa1)
  )

  val intCons = IntAnd(
    IntEqual(Length(StringVariable(4)), IntConst(5)),
    IntEqual(Length(StringVariable(5)), IntConst(5))
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
    val sst = builder.constraintsToSST_Int(relCons, regCons)
    SST.print(sst)

    val pi = SST.getParikhImage(sst)
    pi.foreach(println)

    val z3 = Z3Exp.toZ3Input(intCons, pi)

    println(z3)
  }
}
