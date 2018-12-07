object Main extends App {

  val sst = SSTFactory.getTestSST2()
  sst.toParikhImage.foreach(println)
}