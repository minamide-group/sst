package builderTest

import builder.{ParikhBuilder, SLConsBuilder, SSTBuilder, Z3InputBuilder}
import formula.Conjunction
import formula.atomic.{IntegerEquation, StrInRe, WordEquation}
import formula.integer.{IntC, Operation, StrLen}
import formula.re.StrToRe
import formula.str.{StrReplace, StrV}
import org.scalatest.FlatSpec

class ParikhToZ3_Test extends FlatSpec{

  "for" should "run" in{
    val c1 = WordEquation(StrV("x"), StrReplace(StrV("y"), "p", "r"))
    val c2 = WordEquation(StrV("w"), StrReplace(StrV("z"), "1", "2"))
    val c3 = IntegerEquation(IntC(39), StrLen(StrV("v")), 1)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val c5 = IntegerEquation(StrLen(StrV("x")), Operation(StrLen(StrV("w")), StrLen(StrV("z")), "+"), 3)
    val formula = Conjunction(c5, Conjunction(Conjunction(c1, c2), Conjunction(c3, c4)))

    val (consList, msg) = SLConsBuilder(formula).output

    val cons = consList(0)

    val sstRes = SSTBuilder(cons._1, cons._2, cons._3, '#').output

    val sstList = sstRes._1.get
    val sst = sstRes._2.get

    val semilinear = ParikhBuilder(sst, sstList, cons._1).output
    semilinear.foreach(println)

    val input = Z3InputBuilder(cons._4.toList, semilinear, cons._5).output

    println(input)
  }
}
