package builder

import constraint.atomicSL.{AtomicSLCons, Concatenation}
import constraint.vars.{SST_State, SST_Var, TransState}
import deterministic.boundedcopy.SST
import nondeterministic.Transducer

case class ParikhBuilder(sst: SST[SST_State, Char, Int, SST_Var],
                         sstList: List[SST[SST_State, Char, Char, SST_Var]],
                         atomicCons: List[AtomicSLCons]) {

  type SEMI = Set[(Map[Int, Int], Set[Map[Int, Int]])]

  def output: (Set[SEMI], Transducer[TransState, Char, Map[Int, Int]]) = {
    //(1) there are only integer constraints, so sstList is empty
    //(2) integer constraints contain no variables of sst, in this case sst_int is not constructed
    if (sstList.isEmpty || sst == null)
      (Set(), null)
    else {
      val trans = sst.toMapTransducer
      val parikhs = sst.toParikhImage(trans).map(p => modifyParikhImage(p))
      (parikhs, getTransducer(trans.rename))
    }
  }

  def modifyParikhImage(res0: SEMI): SEMI = {
    //Optimization : Combine regular constraint with previous atomic constraints to reduce the size of later SSTs.
    //Problem : When the last constraint is concatenation, e.x. x2 = x1x0, x2 is represented by x1 and x0
    //Solution : Compute it after Computing Parikh image.
    if (atomicCons.isEmpty)
      return res0
    atomicCons.last match { //the last constraint is concatenation
      case c: Concatenation[Char] if sstList.size == atomicCons.size => { //the last regular constraint is optimized
        val charCount: Int = c.list.collect { case Right(l) => l }.size //symbol in concatenation
      val varsCount: Map[Int, Int] = c.list.collect { case Left(v) => v }.groupBy(identity).map(t => t._1 -> t._2.size)
        res0.map(linear => (addFinalVar(c.left, linear._1, charCount, varsCount), linear._2.map(v => addFinalVar(c.left, v, 0, varsCount))))
      }
      case _ => res0
    }
  }

  def getTransducer(trans0: Transducer[TransState, Char, Map[Int, Int]]): Transducer[TransState, Char, Map[Int, Int]] = {
    if (atomicCons.isEmpty)
      return trans0
    atomicCons.last match { //the last constraint is concatenation
      case c: Concatenation[Char] if sstList.size == atomicCons.size => {
        val states = trans0.states
        val s0 = trans0.s0
        val f = trans0.f
        val charCount: Int = c.list.collect { case Right(l) => l }.size //symbol in concatenation
        val varsCount: Map[Int, Int] = c.list.collect { case Left(v) => v }.groupBy(identity).map(t => t._1 -> t._2.size)
        val delta = trans0.δ.map(t => {
          val origin = t._1
          val end = t._3
          val sigma = t._2
          val vector = if (f(t._3)) {
            addFinalVar(c.left, t._4, charCount, varsCount)
          }
          else {
            addFinalVar(c.left, t._4, 0, varsCount)
          }
          (origin, sigma, end, vector)
        })
        implicit val monoid = trans0.e
        nondeterministic.Transducer(states, s0, delta, f)
      }
      case _ => trans0
    }
  }

  def addFinalVar(idx: Int, idxLength: Map[Int, Int], charCount: Int, varsCount: Map[Int, Int]) = {
    idxLength + (idx -> varsCount.map(t => t._2 * idxLength(t._1)).foldLeft(charCount) { (x, y) => x + y })
  }

}
