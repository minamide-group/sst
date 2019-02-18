package deterministic_test

import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.{Composition, SST}
import deterministic.factory.SSTFactory
import org.scalatest.FlatSpec

class Composition3_Test extends FlatSpec{
  val charSet = Set('a', 'b')
  val factory = SSTFactory(charSet)
  type MySST[X] = SST[SST_State, Char, X, SST_Var]

  def get1()={
    val s0 = SST_State(0, "sst")
    val s1 = SST_State(1, "sst")
    val x = SST_Var(0, "sst")
    val states = Set(s0, s1)
    val f = Map(s1->List[Either[SST_Var, Char]](Left(x)))
    val delta = Map(
      (s0, 'b')->s0,
      (s0, 'a')->s1
    )
    val eta = Map(
      (s0, 'b')->Map(
        x->List(Left(x), Right('b'))
      ),
      (s0, 'a')->Map(
        x->List(Left(x), Right('a'))
      )
    )
    addDefault(
      SST(states, s0, Set(x), delta, eta, f)
    )
  }

  def get2()= factory.reverse

  def addDefault[X](sst: MySST[X]): MySST[X] = {
    val sink = SST_State(-1, sst.s0.name + "sink")
    val vink = sst.vars.map(x => x -> List[Either[SST_Var, X]]()).toMap
    SST(sst.states + sink, sst.s0, sst.vars,
      sst.δ.withDefaultValue(sink),
      sst.η.withDefaultValue(vink),
      sst.f
    )
  }

  "c" should "run" in{
    val sst1 = get1()
    val sst2 = get2()

    val sst = Composition.compose(sst1, sst2).rename("r").trim
    sst.printDetail
  }
}
