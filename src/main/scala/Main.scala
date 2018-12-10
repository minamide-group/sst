object Main extends App {
  val sst = SSTFactory.getTestSST2()
  sst.toSemiLinearSet.foreach(println)
  println(sst.toZ3Input(100, 254))
}