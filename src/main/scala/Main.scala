object Main extends App {

  val sst = SSTFactory.getHalfSST()

  sst.toSemiLinearSet.foreach(println)

  println(sst.toRegExp.eval)
}