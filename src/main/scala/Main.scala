object Main extends App {
  val sst = SSTFactory.getTestSST2()
  sst.toParikhImage.foreach(println)

  val map = Map(
    'a' -> 32,
    'b' -> 29,
    'c' -> 1
  )

  println(sst.toZ3Input(map))
}