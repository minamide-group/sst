package constraint_test

import constraint.SSTBuilder
import constraint.regular.RegCons
import constraint.relational.{Concatenation, RelCons, TransducerConstraint}
import constraint.vars.{FAState, StringVariable, TransState}
import deterministic.boundedcopy.SST
import deterministic.{DFA, Transducer}
import org.scalatest.FlatSpec
import scalaz.Monoid

class ConstraintsToSST_Test extends FlatSpec {

  val dfa1 = DFA(
    Set(FAState(0), FAState(1), FAState(2)),
    FAState(0),
    Map(
      (FAState(0), 'a') -> FAState(1),
      (FAState(1), 'b') -> FAState(2),
      (FAState(2), 'a') -> FAState(0)
    ),
    Set(FAState(2)))


  implicit def strMonoid: Monoid[List[Char]] = new Monoid[List[Char]] {
    def append(f1: List[Char], f2: => List[Char]): List[Char] = f1 ::: f2

    def zero: List[Char] = List()
  }

  val trans = Transducer(Set(
    TransState(0), TransState(1)),
    TransState(0),
    Map(
      (TransState(0), 'a') -> TransState(1),
      (TransState(0), 'b') -> TransState(1),
      (TransState(1), 'a') -> TransState(0),
      (TransState(1), 'b') -> TransState(0)
    ),
    Map(
      (TransState(0), 'a') -> List('a'),
      (TransState(0), 'b') -> List('b'),
      (TransState(1), 'a') -> List(),
      (TransState(1), 'b') -> List()
    ),
    Set(TransState(0), TransState(1))
  )

  val list = List(
    Concatenation(StringVariable(2), StringVariable(1), StringVariable(0)),
    //    TransducerConstraint(StringVariable(3), trans, StringVariable(2)),
    Concatenation(StringVariable(3), StringVariable(1), StringVariable(2)),
    Concatenation(StringVariable(4), StringVariable(1), StringVariable(2)),
    Concatenation(StringVariable(5), StringVariable(1), StringVariable(2)),
    Concatenation(StringVariable(6), StringVariable(3), StringVariable(2)),
    Concatenation(StringVariable(7), StringVariable(1), StringVariable(4)),
    Concatenation(StringVariable(8), StringVariable(6), StringVariable(5)),
    Concatenation(StringVariable(9), StringVariable(6), StringVariable(5)),
    Concatenation(StringVariable(10), StringVariable(6), StringVariable(5)),
    Concatenation(StringVariable(11), StringVariable(6), StringVariable(5)),
    Concatenation(StringVariable(12), StringVariable(6), StringVariable(5)),
    Concatenation(StringVariable(13), StringVariable(6), StringVariable(5)),
    Concatenation(StringVariable(14), StringVariable(6), StringVariable(5)),
    Concatenation(StringVariable(15), StringVariable(6), StringVariable(5)),
  )

  val set = Set[RegCons[FAState, Char]](
    //RegCons(StringVariable(0), dfa1)
  )

  def myAssert(strs: List[String], rels: List[RelCons], regs: Set[RegCons[FAState, Char]]): Unit = {
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
