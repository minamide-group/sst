import regex.MapRegExp._
object Main extends App {

  val sst = SSTFactory.getTestSST()
  sst.toParikhImage.foreach(println)
//  val re = AltExp(
//    ConcatExp(ConcatExp(CharExp(Set((Map(0 -> 2, 1 -> 0),Set()))),StarExp(CharExp(Set((Map(0 -> 2, 1 -> 0),Set()))))),CharExp(Set((Map(0 -> 0, 1 -> 3),Set())))),
//    ConcatExp(ConcatExp(ConcatExp(ConcatExp(CharExp(Set((Map(0 -> 2, 1 -> 0),Set()))),StarExp(CharExp(Set((Map(0 -> 2, 1 -> 0),Set()))))),CharExp(Set((Map(0 -> 0, 1 -> 3),Set())))),StarExp(CharExp(Set((Map(0 -> 0, 1 -> 3),Set()))))),CharExp(Set((Map(0 -> 0, 1 -> 3),Set()))))
//  )
//
//  eval(re) match {
//    case m : CharExp => m.c.foreach(println)
//  }
}