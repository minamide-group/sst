import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.SST
import org.scalatest.FlatSpec

class sstTest extends FlatSpec {

  "sst" should "" in {

    val q = List(SST_State(1, "s"), SST_State(0, "s"))
    val x = SST_Var(0, "x")
    val y = SST_Var(1, "y")

    val delta = Map(
      (q(0), 'a') -> q(1),
      (q(0), 'b') -> q(1),
      (q(1), 'a') -> q(1),
      (q(1), 'b') -> q(1)
    )

    val eta = Map(
      (q(0), 'a') -> Map(x -> List(Left(x), Right('a')), y -> List(Right('a'), Left(y))),
      (q(0), 'b') -> Map(x -> List(Left(x), Right('b')), y -> List(Right('b'), Left(y))),
      (q(1), 'a') -> Map(x -> List(Left(x), Right('a')), y -> List(Right('a'), Left(y))),
      (q(1), 'b') -> Map(x -> List(Left(x), Right('b')), y -> List(Right('b'), Left(y)))
    )

    val f = Map(q(1) -> List(Left(x), Right('b'), Left(y)))

    val sst = SST(q.toSet, q(0), Set(x, y), delta, eta, f)

    val trans = sst.toMapTransducer.rename

    trans.print
  }

  "t" should "" in {
    val l = List(1, 2, 3, 4, 5)
    val l2 = l.collect {
      case x if x % 2 == 0 => x
    }
    println(l2)
  }
}
