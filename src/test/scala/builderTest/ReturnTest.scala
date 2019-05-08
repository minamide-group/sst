package builderTest

import org.scalatest.FlatSpec

class ReturnTest extends FlatSpec{


  "a" should "run" in{
    println(f(3))
  }

  def f(x : Int): Int ={
    if(x>0)
      return 1
    0
  }
}
