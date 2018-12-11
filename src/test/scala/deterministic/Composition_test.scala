package deterministic.copyless

import org.scalatest._
class Composition_test extends FlatSpec{
  val rev = {
    val q0 = 0
    val x0 = 'x'

    val delta = Map(
      (q0,'a')->q0,
      (q0,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Right('a'), Left(x0))
      ),
      (q0,'b')->Map(
        x0->List(Right('b'),Left(x0))
      )
    )

    val f = Map(q0->List(Left(x0)))
    SST(Set(q0), q0, Set(x0), delta, eta, f)
  }

  val half1 = {
    val q0 = 0
    val q1 = 1
    val x0 = 'x'

    val delta = Map(
      (q0,'a')->q1,
      (q0,'b')->q1,
      (q1,'a')->q0,
      (q1,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x0))
      ),
      (q0,'b')->Map(
        x0->List(Left(x0))
      ),
      (q1,'a')->Map(
        x0->List(Left(x0), Right('a'))
      ),
      (q1,'b')->Map(
        x0->List(Left(x0), Right('b'))
      )
    )

    val f = Map(q0->List(Left(x0)), q1->List(Left(x0)))
    SST(Set(q0, q1), q0, Set(x0), delta, eta, f)
  }

  val half2 = {
    val q0 = 0
    val x0 = 'x'
    val x1 = 'y'

    val delta = Map(
      (q0,'a')->q0,
      (q0,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x1), Right('a')),
        x1->List(Left(x0))
      ),
      (q0,'b')->Map(
        x0->List(Left(x1), Right('b')),
        x1->List(Left(x0))
      ),
    )

    val f = Map(q0->List(Left(x1)))
    SST(Set(q0), q0, Set(x0, x1), delta, eta, f)
  }

  val dup = {
    val q0 = 0
    val x0 = 'x'
    val x1 = 'y'

    val delta = Map(
      (q0,'a')->q0,
      (q0,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        x0->List(Left(x0), Right('a')),
        x1->List(Left(x1), Right('a'))
      ),
      (q0,'b')->Map(
        x0->List(Left(x0), Right('b')),
        x1->List(Left(x1), Right('b'))
      ),
    )

    val f = Map(q0->List(Left(x0), Left(x1)))
    SST(Set(q0), q0, Set(x0, x1), delta, eta, f)
  }

  val f1 = {
    val q0 = 0
    val q1 = 1
    val a = Right('a')
    val b = Right('b')
    val x = Left('x')
    val y = Left('y')
    val z = Left('z')
    
    val delta = Map(
      (q0,'a')->q1,
      (q0,'b')->q0,
      (q1,'a')->q1,
      (q1,'b')->q0,
    )

    val eta = Map(
      (q0,'a')->Map(
        'x'->List(x, a),
        'y'->List(a, y, a),
        'z'->List(x)
      ),
      (q0,'b')->Map(
        'x'->List(x, b),
        'y'->List(),
        'z'->List()
      ),
      (q1,'a')->Map(
        'x'->List(x, a),
        'y'->List(a, y, a),
        'z'->List(z)
      ),
      (q1,'b')->Map(
        'x'->List(z, y, b),
        'y'->List(),
        'z'->List()
      ),
    )

    val f = Map(q1->List(x))
    SST(Set(q0, q1), q0, Set('x', 'y', 'z'), delta, eta, f)
  }


  def getRandomString(length: Int, chars: List[Char]): Seq[Char] = {
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (_ <- 0 to length-1)
      sb.append(chars(r.nextInt(chars.size)))
    sb.toSeq
  }

  // def eval[X, Γ](expr:List[Either[X, Γ]], env:Map[X, List[Either[X, Γ]]]):List[Either[X, Γ]]={
  //   def _eval(expr:List[Either[X, Γ]], ret: List[Either[X, Γ]]):List[Either[X, Γ]]={
  //     expr match {
  //       case next::rest =>{
  //         next match {
  //           case Left(x)=>  _eval(rest, ret:::env(x))
  //           case Right(gamma)=> _eval(rest, ret:+Right(gamma))
  //         }
  //       }
  //       case _ => ret
  //     }
  //   }
  //   _eval(expr, List())
  // }


  def inspectSST[Q, A, B, X](sst: SST[Q, A, B, X]) {
    println(sst.states.size, "states: ", sst.states)
    println(sst.vars.size, "vars:   ", sst.vars)
    println(sst.δ.size, "delta:  ", sst.δ)
    println(sst.η.size, "eta:    ", sst.η)
    println(sst.f.size, "F:      ", sst.f)
  }

  def inspectMonoidSST[Q, A, B, X, Y](msst: MonoidSST[Q, A, B, X, Y]) {
    inspectSST(msst.sst)
    println("vars2:   ", msst.vars2)
    println("F2:      ", msst.final2)
  }

  def resultToOption[Q, A](result: (Boolean, Q, Seq[A])): Option[Seq[A]] = {
    if (result._1) {
      Some(result._3)
    } else {
      None
    }
  }

  def testComposition[Q, R, X, Y](sst1: SST[Q, Char, Char, X], sst2: SST[R, Char, Char, Y]) {
    val comp = Composition.compose(sst1, sst2)
    val r = new scala.util.Random

    for (_ <- 0 to 100) {
      val input = getRandomString(r.nextInt(100), List('a', 'b'))
      val result = resultToOption(comp.process(input))
      val expected = resultToOption(sst1.process(input)) flatMap (x => resultToOption(sst2.process(x)))

      assert(result.mkString == expected.mkString)
    }
  }

  "composition" should "output the same" in {
    val list = List(rev, half1, half2, dup)
    for (sst1 <- list; sst2 <- list) {
      testComposition(sst1, sst2)
    }
  }

  "calcBoundedness" should "stop with correct result" in {
    assert(Composition.calcBoundedness(rev) == 1)
    assert(Composition.calcBoundedness(half1) == 1)
    assert(Composition.calcBoundedness(half2) == 1)
    assert(Composition.calcBoundedness(dup) == 1)
    assert(Composition.calcBoundedness(f1) == 2)
  }

  // "sst1.trans" should "output function" in{
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)+1
  //     val str = getRandomString(length,List('a','b'))
  //     val result = sst1.trans(str)(sst1.s0)
  //     assert(sst1.f.contains(result._1))
  //     val outputStr = eval(sst1.f(result._1), result._2).filter(x=>x.isRight).map(x=>x.right.get).mkString
  //     assert(outputStr == str + str.reverse)
  //   }
  // }

  // "sst2.process" should "output the alphabet at even index in w" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)
  //     val str = getRandomString(length,List('a','b'))
  //     val ret1 = sst2.process(str)
  //     assert(ret1._1==true)
  //     assert(ret1._3.mkString == str.indices.collect{case i if i%2==0 =>str(i)}.mkString)
  //   }
  //   val ret2 = sst2.process("abc")
  //   assert(ret2._1==false)
  // }

  // "sst2.trans" should "output function" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)+1
  //     val str = getRandomString(length,List('a','b'))
  //     val result = sst2.trans(str)(sst2.s0)
  //     assert(sst2.f.contains(result._1))
  //     val outputStr = eval(sst2.f(result._1), result._2).filter(x=>x.isRight).map(x=>x.right.get).mkString
  //     assert(outputStr == str.indices.collect{case i if i%2==0 =>str(i)}.mkString)
  //   }
  // }

  // "sst3.process" should "delete all the 'a' in w" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)
  //     val str = getRandomString(length,List('a','b'))
  //     val ret1 = sst3.process(str)
  //     assert(ret1._1==true)
  //     assert(ret1._3.mkString == str.filterNot(x=>x=='a').mkString)
  //   }
  //   val ret2 = sst3.process("abc")
  //   assert(ret2._1==false)
  // }

  // "sst3.trans" should "output function" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)+1
  //     val str = getRandomString(length,List('a','b'))
  //     val result = sst3.trans(str)(sst3.s0)
  //     assert(sst3.f.contains(result._1))
  //     val outputStr = eval(sst3.f(result._1), result._2).filter(x=>x.isRight).map(x=>x.right.get).mkString
  //     assert(outputStr == str.filterNot(x=>x=='a').mkString)
  //   }
  // }
}
