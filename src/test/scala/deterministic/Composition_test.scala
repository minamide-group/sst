package deterministic.copyless

import org.scalatest._
class Composition_test extends FlatSpec{

  val sst1 = SSTFactory.getReverseSST()
  val sst2 = SSTFactory.getHalfSST()
  val sst3 = SSTFactory.getDeletionSST()
  val sst4 = SSTFactory.getCPSST()

  // def getRandomString(length: Int, chars: List[Char]):String = {
  //   val r = new scala.util.Random
  //   val sb = new StringBuilder
  //   for (_ <- 0 to length-1)
  //     sb.append(chars(r.nextInt(chars.size)))
  //   sb.toString
  // }

  // def eval[X, Γ](expr:List[Either[X, Γ]], env:Map[X, List[Either[X, Γ]]]):List[Either[X, Γ]]={
  //   def _eval(expr:List[Either[X, Γ]], ret: List[Either[X, Γ]]):List[Either[X, Γ]]={
  //     expr match {
  //       case next::rest =>{
  //         next match {
  //           case Left(x)=>  _eval(rest, ret:::env(x))
  //           case Right(gamma)=> _eval(rest, ret:+Right(gamma))
  //         }
  //       }
  //       case _ => ret
  //     }
  //   }
  //   _eval(expr, List())
  // }

  "composition" should "terminate" in {
    Composition.composeToMonoidSST(sst1, sst2)
    Composition.composeToMonoidSST(sst2, sst3)
    Composition.composeToMonoidSST(sst3, sst4)
    Composition.composeToMonoidSST(sst4, sst1)    
  }

  // "sst1.process" should "output w w_reverse" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)+1
  //     val str = getRandomString(length,List('a','b'))
  //     val ret1 = sst1.process(str)
  //     assert(ret1._1==true)
  //     assert(ret1._3.mkString == str+str.reverse)
  //   }
  //   val ret2 = sst1.process("abc")
  //   assert(ret2._1==false)
  //   val ret3 = sst1.process("")
  //   assert(ret3._1==false)
  // }

  // "sst1.trans" should "output function" in{
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)+1
  //     val str = getRandomString(length,List('a','b'))
  //     val result = sst1.trans(str)(sst1.s0)
  //     assert(sst1.f.contains(result._1))
  //     val outputStr = eval(sst1.f(result._1), result._2).filter(x=>x.isRight).map(x=>x.right.get).mkString
  //     assert(outputStr == str + str.reverse)
  //   }
  // }

  // "sst2.process" should "output the alphabet at even index in w" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)
  //     val str = getRandomString(length,List('a','b'))
  //     val ret1 = sst2.process(str)
  //     assert(ret1._1==true)
  //     assert(ret1._3.mkString == str.indices.collect{case i if i%2==0 =>str(i)}.mkString)
  //   }
  //   val ret2 = sst2.process("abc")
  //   assert(ret2._1==false)
  // }

  // "sst2.trans" should "output function" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)+1
  //     val str = getRandomString(length,List('a','b'))
  //     val result = sst2.trans(str)(sst2.s0)
  //     assert(sst2.f.contains(result._1))
  //     val outputStr = eval(sst2.f(result._1), result._2).filter(x=>x.isRight).map(x=>x.right.get).mkString
  //     assert(outputStr == str.indices.collect{case i if i%2==0 =>str(i)}.mkString)
  //   }
  // }

  // "sst3.process" should "delete all the 'a' in w" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)
  //     val str = getRandomString(length,List('a','b'))
  //     val ret1 = sst3.process(str)
  //     assert(ret1._1==true)
  //     assert(ret1._3.mkString == str.filterNot(x=>x=='a').mkString)
  //   }
  //   val ret2 = sst3.process("abc")
  //   assert(ret2._1==false)
  // }

  // "sst3.trans" should "output function" in {
  //   val r = new scala.util.Random
  //   for(_ <-0 to 100){
  //     val length = r.nextInt(1000)+1
  //     val str = getRandomString(length,List('a','b'))
  //     val result = sst3.trans(str)(sst3.s0)
  //     assert(sst3.f.contains(result._1))
  //     val outputStr = eval(sst3.f(result._1), result._2).filter(x=>x.isRight).map(x=>x.right.get).mkString
  //     assert(outputStr == str.filterNot(x=>x=='a').mkString)
  //   }
  // }
}
