
object Main extends App {

  val sst = SSTFactory.getTestSST()
  sst.toParikhImage.foreach(println)
}