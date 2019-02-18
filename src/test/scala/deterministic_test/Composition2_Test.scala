package deterministic_test

import constraint.vars.{SST_State, SST_Var}
import deterministic.boundedcopy.{Composition, SST}
import org.scalatest.FlatSpec

class Composition2_Test extends FlatSpec{

  class S(i : Int)
  class X(i : Int)
  type MySST[X] = SST[SST_State, Char, X, SST_Var]

  def getS3:SST[S,Char,Char,X] = {
    val s = List.range(0,4).map(i=>new S(i))
    val v = List.range(0,3).map(i=>new X(i))

    val sink = new S(-1)
    val vink = v.map(x=> x->List[Either[X,Char]]() ).toMap

    val delta = Map(
      (s(0), 'a')->s(0),
      (s(0), 'b')->s(0),
      (s(0), '#')->s(1),

      (s(1), 'a')->s(1),
      (s(1), 'b')->s(1),
      (s(1), '#')->s(2),

      (s(2), 'a')->s(2),
      (s(2), 'b')->s(2),
      (s(2), '#')->s(3)
    ).withDefaultValue(sink)

    val eta = Map(
      (s(0), 'a')->Map(
        v(0)->List(Left(v(0)), Right('a')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      ),
      (s(0), 'b')->Map(
        v(0)->List(Left(v(0)), Right('b')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      ),
      (s(0), '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      ),

      (s(1), 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('a')),
        v(2)->List(Left(v(2)))
      ),
      (s(1), 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('b')),
        v(2)->List(Left(v(2)))
      ),
      (s(1), '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      ),

      (s(2), 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('a'))
      ),
      (s(2), 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('b'))
      ),
      (s(2), '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      )
    ).withDefaultValue(vink)

    val f = Map(s(3)->
      List(Left(v(0)), Right('#'), Left(v(1)), Right('#'), Left(v(2)), Right('#'), Left(v(0)), Left(v(2)), Right('#'))
    )

    SST(s.toSet+sink,
      s(0),
      v.toSet,
      delta,
      eta,
      f
    )
  }

  def getS3f :SST[S,Char,Char,X]= {
    val p0 = new S(0)
    val p1 = new S(1)
    val q1 = new S(2)
    val q2 = new S(3)
    val q3 = new S(4)
    val sink = new S(-1)

    val v = List.range(0,3).map(i=> new X(i))

    val vink = v.map(x=> x->List[Either[X,Char]]() ).toMap

    val delta = Map(
      (p0, 'a')->p1,

      (p1,'b')->p1,
      (p1,'#')->q1,


      (q1, 'a')->q1,
      (q1, 'b')->q1,
      (q1, '#')->q2,

      (q2, 'a')->q2,
      (q2, 'b')->q2,
      (q2, '#')->q3
    ).withDefaultValue(sink)

    val eta = Map(
      (p0, 'a')->Map(
        v(0)->List(Left(v(0)), Right('a')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      ),

      (p1, 'b')->Map(
        v(0)->List(Left(v(0)), Right('b')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      ),
      (p1, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      ),

      (q1, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('a')),
        v(2)->List(Left(v(2)))
      ),
      (q1, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('b')),
        v(2)->List(Left(v(2)))
      ),
      (q1, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      ),

      (q2, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('a'))
      ),
      (q2, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('b'))
      ),
      (q2, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)))
      )
    ).withDefaultValue(vink)

    val f = Map(q3->
      List(Left(v(0)), Right('#'), Left(v(1)), Right('#'), Left(v(2)), Right('#'), Left(v(0)), Left(v(2)), Right('#'))
    )

    SST(Set(p0,p1,q1,q2,q3,sink),
      p0,
      v.toSet,
      delta,
      eta,
      f
    )
  }

  def getS4:SST[S,Char,Char,X] = {
    val s = List.range(0,5).map(i=>new S(i))
    val v = List.range(0,5).map(i=>new X(i))

    val sink = new S(-1)
    val y = new X(-1)
    val vink = (y::v).map(x=> x->List[Either[X,Char]]() ).toMap

    val delta : Map[(S,Char),S]= Map(
      (s(0), 'a')->s(0),
      (s(0), 'b')->s(0),
      (s(0), '#')->s(1),

      (s(1), 'a')->s(1),
      (s(1), 'b')->s(1),
      (s(1), '#')->s(2),

      (s(2), 'a')->s(2),
      (s(2), 'b')->s(2),
      (s(2), '#')->s(3),

      (s(3), 'a')->s(3),
      (s(3), 'b')->s(3),
      (s(3), '#')->s(4)
    ).withDefaultValue(sink)

    val eta : Map[(S, Char), Map[X, List[Either[X,Char]]]] = Map(
      (s(0), 'a')->Map(
        v(0)->List(Left(v(0)), Right('a')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),
      (s(0), 'b')->Map(
        v(0)->List(Left(v(0)), Right('b')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),
      (s(0), '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),

      (s(1), 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('a')),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),
      (s(1), 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('b')),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),
      (s(1), '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),

      (s(2), 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('a')),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),
      (s(2), 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('b')),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),
      (s(2), '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4))),
        y->List(Left(y))
      ),

      (s(3), 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3)),Right('a')),
        v(4)->List(Left(v(4))),
        y->List(Right('a'),Left(y))
      ),
      (s(3), 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3)),Right('b')),
        v(4)->List(Left(v(4))),
        y->List(Right('b'),Left(y))
      ),
      (s(3), '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(y)),
        y->List(Left(y))
      )
    ).withDefaultValue(vink)

    val f = Map(s(4)->
      List(Left(v(0)), Right('#'), Left(v(1)), Right('#'), Left(v(2)), Right('#'), Left(v(3)), Right('#'), Left(v(4)), Right('#'))
    )

    SST(s.toSet+sink,
      s(0),
      v.toSet+y,
      delta,
      eta,
      f
    )
  }

  def getS5f_2nd:SST[S,Char,Char,X] = {
    val p0 = new S(0)
    val p1 = new S(1)
    val q1 = new S(2)
    val q2 = new S(3)
    val q3 = new S(4)
    val r0 = new S(5)
    val r1 = new S(6)
    val q5 = new S(7)
    val sink = new S(-1)

    val v = List.range(0,5).map(i=> new X(i))

    val vink = v.map(x=> x->List[Either[X,Char]]() ).toMap

    val delta = Map(
      (p0, 'a')->p1,

      (p1,'b')->p1,
      (p1,'#')->q1,


      (q1, 'a')->q1,
      (q1, 'b')->q1,
      (q1, '#')->q2,

      (q2, 'a')->q2,
      (q2, 'b')->q2,
      (q2, '#')->q3,

      (q3, 'a')->q3,
      (q3, 'b')->q3,
      (q3, '#')->r0,

      (r0, 'b')->r0,
      (r0, 'a')->r1,

      (r1, '#')->q5
    ).withDefaultValue(sink)

    val eta = Map(
      (p0, 'a')->Map(
        v(0)->List(Left(v(0)), Right('a')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (p1, 'b')->Map(
        v(0)->List(Left(v(0)), Right('b')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (p1, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (q1, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('a')),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q1, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('b')),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q1, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (q2, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('a')),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q2, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('b')),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q2, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (q3, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3)),Right('a')),
        v(4)->List(Left(v(4)))
      ),
      (q3, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3)), Right('b')),
        v(4)->List(Left(v(4)))
      ),
      (q3, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (r0, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)),Right('b'))
      ),
      (r0, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)),Right('a'))
      ),

      (r1, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      )

    ).withDefaultValue(vink)

    val f = Map(q5->
      List(Left(v(0)), Right('#'), Left(v(1)), Right('#'), Left(v(2)), Right('#'), Left(v(3)), Right('#'), Left(v(4)), Right('#'))
    )

    SST(Set(p0,p1,q1,q2,q3,r0,r1,q5, sink),
      p0,
      v.toSet,
      delta,
      eta,
      f
    )
  }

  def getS5f_1st:SST[S,Char,Char,X] = {
    val q0 = new S(0)
    val q1 = new S(2)
    val q2 = new S(3)
    val q3 = new S(4)
    val r0 = new S(5)
    val r1 = new S(6)
    val q5 = new S(7)
    val sink = new S(-1)

    val v = List.range(0,5).map(i=> new X(i))

    val vink = v.map(x=> x->List[Either[X,Char]]() ).toMap

    val delta = Map(
      (q0, 'a')->q0,
      (q0,'b')->q0,
      (q0,'#')->q1,

      (q1, 'a')->q1,
      (q1, 'b')->q1,
      (q1, '#')->q2,

      (q2, 'a')->q2,
      (q2, 'b')->q2,
      (q2, '#')->q3,

      (q3, 'a')->q3,
      (q3, 'b')->q3,
      (q3, '#')->r0,

      (r0, 'b')->r0,
      (r0, 'a')->r1,

      (r1, '#')->q5
    ).withDefaultValue(sink)

    val eta = Map(
      (q0, 'a')->Map(
        v(0)->List(Left(v(0)), Right('a')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q0, 'b')->Map(
        v(0)->List(Left(v(0)), Right('b')),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q0, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (q1, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('a')),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q1, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1)), Right('b')),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q1, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (q2, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('a')),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q2, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2)),Right('b')),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),
      (q2, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (q3, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3)),Right('a')),
        v(4)->List(Left(v(4)))
      ),
      (q3, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3)), Right('b')),
        v(4)->List(Left(v(4)))
      ),
      (q3, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      ),

      (r0, 'b')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)),Right('b'))
      ),
      (r0, 'a')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)),Right('a'))
      ),

      (r1, '#')->Map(
        v(0)->List(Left(v(0))),
        v(1)->List(Left(v(1))),
        v(2)->List(Left(v(2))),
        v(3)->List(Left(v(3))),
        v(4)->List(Left(v(4)))
      )

    ).withDefaultValue(vink)

    val f = Map(q5->
      List(Left(v(0)), Right('#'), Left(v(1)), Right('#'), Left(v(2)), Right('#'), Left(v(3)), Right('#'), Left(v(4)), Right('#'))
    )

    SST(Set(q0,q1,q2,q3,r0,r1,q5, sink),
      q0,
      v.toSet,
      delta,
      eta,
      f
    )
  }

  def addDefault[X](sst: MySST[X]): MySST[X] = {
    val sink = SST_State(-1, sst.s0.name + "sink")
    val vink = sst.vars.map(x => x -> List[Either[SST_Var, X]]()).toMap
    SST(sst.states + sink, sst.s0, sst.vars,
      sst.δ.withDefaultValue(sink),
      sst.η.withDefaultValue(vink),
      sst.f
    )
  }

  "1st" should "run" in {
    //43 states, 7 vars
    //4.772s
    val s3 = getS3f
    val s4 = getS4
    val s5 = getS5f_1st

    val s34 = addDefault(Composition.compose(s3,s4).rename("r11"))
    s34.print
    val s345 = Composition.compose(s34, s5).rename("r12")
    s345.print
  }

  "2nd" should "run" in {
    //63 states
    //7  vars
    //22.235s
    val s3 = getS3
    val s4 = getS4
    val s5 = getS5f_2nd

    val s34 = addDefault(Composition.compose(s3,s4).trim.rename("r11"))
    s34.print
    val s345 = Composition.compose(s34, s5).trim.rename("r12")
    s345.print
  }
}
