package builderTest

import builder.{ParikhBuilder, SLConsBuilder, SSTBuilder}
import formula.Conjunction
import formula.atomic.{IntegerEquation, StrInRe, WordEquation}
import formula.integer.{IntC, StrLen}
import formula.re.StrToRe
import formula.str.{StrReplace, StrV}
import org.scalatest.FlatSpec

class SSTtoParikh_Test extends FlatSpec{

  "for" should "run" in{
    val c1 = WordEquation(StrV("x"), StrReplace(StrV("y"), "p", "r"))
    val c2 = WordEquation(StrV("w"), StrReplace(StrV("z"), "1", "2"))
    val c3 = IntegerEquation(IntC(39), StrLen(StrV("v")), 1)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val formula = Conjunction(Conjunction(c1, c2), Conjunction(c3, c4))

    val (consList, msg) = SLConsBuilder(formula).output

    val cons = consList(0)

    val sstRes = SSTBuilder(cons._1, cons._2, cons._3, '#').output

    val sstList = sstRes._1.get
    val sst = sstRes._2.get

    val semilinear = ParikhBuilder(sst, sstList, cons._1).output
    println(cons._5)
    semilinear.foreach(println)
  }
}
