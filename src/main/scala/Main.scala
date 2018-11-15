object Main extends App {

  val sst = SSTFactory.getReverseSST()

  sst.toSemiLinearSet.foreach(println)
}