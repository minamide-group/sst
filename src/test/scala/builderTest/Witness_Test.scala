package builderTest

import scala.sys.process._

import builder.{SLConsBuilder, SSTBuilder, WitnessBuilder}
import formula.Conjunction
import formula.atomic.{StrInRe, WordEquation}
import formula.re.StrToRe
import formula.str.{StrConcat, StrReverse, StrV}
import org.scalatest.FlatSpec

class Witness_Test extends FlatSpec{

  "witness" should "run" in{
    val p1 = WordEquation(StrV("x1"), StrConcat(List(Left(StrV("x0")), Left(StrV("x0")))))
    val p2 = WordEquation(StrV("x2"), StrReverse(StrV("x1")))
    val p3 = StrInRe(StrV("x1"), StrToRe("aaaa"), false)
    val p = Conjunction(p1, Conjunction(p2, p3))

    val split = 0.toChar

    val (we, sr, chars, _, map)  = SLConsBuilder(p).output.head

    val sstList = SSTBuilder(we, sr, chars, split, 3, false, false).constraintsToSSTs(we, sr).get
    //sstList.foreach(_.printDetail)

    val sst = SSTBuilder(we, sr, chars, 0.toChar, 3, false, false).compose(sstList.head, sstList.last)
    //sst.printDetail

    val wb = WitnessBuilder("", map, sst, null, chars, split)
    println(wb.output)
  }

  "witness_x0=aa" should "run" in{
    val p0 = WordEquation(StrV("x0"), StrConcat(List(Right("aa"))))
    val p1 = WordEquation(StrV("x1"), StrConcat(List(Left(StrV("x0")), Left(StrV("x0")))))
    val p2 = WordEquation(StrV("x2"), StrReverse(StrV("x1")))
    val p3 = StrInRe(StrV("x1"), StrToRe("aaaa"), false)
    val p = Conjunction(p0,Conjunction(p1, Conjunction(p2, p3)))

    val split = 0.toChar

    val (we, sr, chars, _, map)  = SLConsBuilder(p).output.head
    //println(map)

    val sstList = SSTBuilder(we, sr, chars, split, 3, false, false).constraintsToSSTs(we, sr).get
    //println(sstList.size)
    //sstList.foreach(_.printDetail)


    val sst_0 = SSTBuilder(we, sr, chars, 0.toChar, 3, false, false).compose(sstList.head, sstList(1))
    val sst = SSTBuilder(we, sr, chars, 0.toChar, 3, false, false).compose(sst_0, sstList(2))
    sst.printDetail
    //println(sst.process(""))

    val wb = WitnessBuilder("", map, sst, null, chars, split)
    println(wb.output)
  }


  "z3" should "run" in{
    val path = "C:\\Users\\leaf6\\IdeaProjects\\Automata\\out\\artifacts\\checker\\int.z3"

    val output = ("z3 -smt2 " + path).!!

    println(output)
  }

}
