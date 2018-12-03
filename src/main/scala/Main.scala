object Main extends App {

  val sst = SSTFactory.getBoundedCopySST()

  val regex = sst.toRegExp.eval

  println(regex)
}