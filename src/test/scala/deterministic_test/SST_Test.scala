package deterministic_test

import deterministic.factory.SSTFactory
import org.scalatest.FlatSpec

class SST_Test extends FlatSpec {

  val factory = SSTFactory(Set('a', 'b', 'c', 'd'))

  "replaceAll" should "run" in {
    val chars = factory.charSet.toList
    for(_ <- 0 to 1000){
      val str1 = getRandomString(5, chars)
      val str2 = getRandomString(5, chars)
      val sst = factory.replaceAll(str1, str2)
      for(_<- 0 to 1000) {
        val input = getRandomString(100, chars)
        val output = sst.process(input)._3.mkString
        val expected = input.replaceAll(str1, str2)
        assert(output == expected)
      }
    }
  }

  "replaceFirst" should "run" in {
    val chars = factory.charSet.toList

    for(_ <- 0 to 1000){
      val str1 = getRandomString(6, chars)
      val str2 = getRandomString(3, chars)
      val sst = factory.replaceFirst(str1, str2)
      for(_ <- 0 to 1000) {
        val input = getRandomString(100, chars)
        val output = sst.process(input)._3.mkString
        val expected = input.replaceFirst(str1, str2)
        assert(output == expected)
      }
    }
  }

  "next" should "run" in {
    val next = factory.getNext("acabacad")
    val enext = factory.extendNext(next)
    println(next)
    println(enext)
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
