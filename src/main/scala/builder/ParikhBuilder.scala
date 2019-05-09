package builder

import constraint.atomicSL.{AtomicSLCons, Concatenation}
import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.SST

case class ParikhBuilder[Σ](sst : SST[SST_State, Σ, Int, SST_Var],
                            sstList : List[SST[SST_State, Σ, Σ, SST_Var]],
                            atomicCons: List[AtomicSLCons]) {

  def output = {
    getParikhImage(sst, sstList, atomicCons)
  }

  type MySST[X] = SST[SST_State, Σ, X, SST_Var]

  def getParikhImage(sst: MySST[Int], sstList: List[MySST[Σ]], atomicCons: List[AtomicSLCons]):Set[(Map[Int,Int], Set[Map[Int,Int]])] = {
    def f(idx: Int, idxLength: Map[Int, Int], charCount: Int, varsCount: Map[Int, Int]) = {
      idxLength + (idx -> (charCount + varsCount.map(t => t._2 * idxLength(t._1)).reduce(_ + _)))
    }
    //Optimization : Combine regular constraint with previous atomic constraints to reduce the size of later SSTs.
    //Problem : When the last constraint is concatenation, e.x. x2 = x1x0, x2 is represented by x1 and x0
    //Solution : Compute it after Computing Parikh image.
    val res0 = sst.toParikhImage
    if(atomicCons.isEmpty)
      return res0
    atomicCons.last match { //the last constraint is concatenation
      case c: Concatenation[Σ] if sstList.size == atomicCons.size => { //the last regular constraint is optimized
        val charCount : Int = c.list.collect { case Right(l) => l }.size        //symbol in concatenation
        val varsCount : Map[Int, Int] = c.list.collect { case Left(v) => v }.groupBy(identity).map(t => t._1 -> t._2.size)
        res0.map(linear => (f(c.left, linear._1, charCount, varsCount), linear._2.map(v => f(c.left, v, 0, varsCount))))
      }
      case _ => res0
    }
  }

}
