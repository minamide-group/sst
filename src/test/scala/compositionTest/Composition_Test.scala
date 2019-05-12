package compositionTest

import builder.SSTBuilder
import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.SST
import org.scalatest.FlatSpec

class Composition_Test extends FlatSpec{

  val sst1 = SST[SST_State, Char, Char, SST_Var](
    Set(SST_State(0, "s1")),
    SST_State(0, "s1"),
    Set(),
    Map(),
    Map(),
    Map(SST_State(0, "s1")->List(Right('a')))
  )

  val sst2 = SST[SST_State, Char, Char, SST_Var](
    Set(SST_State(0, "s2"), SST_State(1, "s2")),
    SST_State(0, "s2"),
    Set(),
    Map((SST_State(0, "s2"), 'a')-> SST_State(1, "s2")),
    Map(),
    Map(SST_State(1, "s2")->List(Right('a')))
  )

  "compo" should "run" in{
    val sst = SSTBuilder(null,null, Set('a'), '#', 0, false, false).compose(sst1, sst2)
    print(sst)
  }
}
