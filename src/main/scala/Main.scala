object Main extends App {

  val sst = SSTFactory.getBoundedCopySST()

  println(sst.toSemiLinearSet)
}