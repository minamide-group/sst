package deterministic_test

import deterministic.examples
import deterministic.examples.SSTExamples
import org.scalatest.FlatSpec

class SST_Test extends FlatSpec {

  "half sst" should "run" in {
    val sst = SSTExamples.getHalfSST()
    val r = new scala.util.Random
    for (_ <- 0 to 100) {
      val length = r.nextInt(1000)
      val str = getRandomString(length, List('a', 'b'))
      val ret1 = sst.process(str)
      assert(ret1._1 == true)
      assert(ret1._3.mkString == str.indices.collect { case i if i % 2 == 0 => str(i) }.mkString)
    }
    val ret2 = sst.process("abc")
    assert(ret2._1 == false)
  }

  "half sst" should "output function" in {
    val sst = examples.SSTExamples.getHalfSST()
    val r = new scala.util.Random
    for (_ <- 0 to 100) {
      val length = r.nextInt(1000) + 1
      val str = getRandomString(length, List('a', 'b'))
      val result = sst.trans(str)(sst.s0)
      assert(sst.f.contains(result._1))
      val outputStr = eval(sst.f(result._1), result._2).filter(x => x.isRight).map(x => x.right.get).mkString
      assert(outputStr == str.indices.collect { case i if i % 2 == 0 => str(i) }.mkString)
    }
  }

  def eval[X, Γ](expr: List[Either[X, Γ]], env: Map[X, List[Either[X, Γ]]]): List[Either[X, Γ]] = {
    def _eval(expr: List[Either[X, Γ]], ret: List[Either[X, Γ]]): List[Either[X, Γ]] = {
      expr match {
        case next :: rest => {
          next match {
            case Left(x) => _eval(rest, ret ::: env(x))
            case Right(gamma) => _eval(rest, ret :+ Right(gamma))
          }
        }
        case _ => ret
      }
    }

    _eval(expr, List())
  }

  def getRandomString(length: Int, chars: List[Char]): String = {
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (_ <- 0 to length - 1)
      sb.append(chars(r.nextInt(chars.size)))
    sb.toString
  }


}
