import org.scalatest._
//import src.main.scala.simprocessing.MinMaxFinder.scala

class TestMinMaxFinder extends FlatSpec with Matchers {
  val x = (-0.9 to 10.2 by 0.1).toSeq
  val window = 5
  
  "MinMaxFinder" should "return 0,2,4,6,8,10,... when called on cos(pi*x/2)" in {
    val y = x.map(z => math.cos(z*math.Pi/2))
    val res = simprocessing.MinMaxFinder.useLocalExtrema(x,y,window).map(z => z.location)
    for (i <- 0 until res.size){
      //println(res(i))
      res(i) shouldBe (i*2.0 +- 0.01)
    }
  }
  
  it should "return 1,3,5,7,9... when called on sin(pi*x/2)" in {
    val y = x.map(z => math.sin(z*math.Pi/2))
    val res = simprocessing.MinMaxFinder.useLocalExtrema(x,y,window).map(z => z.location)
    for (i <- 0 until res.size){
      //println(res(i))
      res(i) shouldBe (1.0 + i*2.0 +- 0.01)
    }
  }

  it should "return 1.0 when called on x^2 - 2x" in {
    val y = x.map(z => z*z - 2*z)
    val res = simprocessing.MinMaxFinder.useLocalExtrema(x,y,window).map(z => z.location)
    //res.foreach(println(_))
    res.size shouldBe 1
    res(0) shouldBe (1.0 +- 0.01)
  }

  it should "return 2/3,2.0 when called on x^3 - 4x^2 + 4x" in {
    val y = x.map(z => z*z*z - 4*z*z + 4*z)
    val res = simprocessing.MinMaxFinder.useLocalExtrema(x,y,window).map(z => z.location)
    //res.foreach(println(_))
    res.size shouldBe 2
    res(0) shouldBe (2.0/3 +- 0.01)
    res(1) shouldBe (2.0 +- 0.01)
  }

  it should "return 0.691,3.0,5.309 when called on (0.3x^2-1.8x+1.1)^2" in {
    val y = x.map(z => math.pow(0.3*z*z - 1.8*z + 1.1, 2))
    val res = simprocessing.MinMaxFinder.useLocalExtrema(x,y,window).map(z => z.location)
    //res.foreach(println(_))
    res.size shouldBe 3
    res(0) shouldBe (0.691 +- 0.01)
    res(1) shouldBe (3.0 +- 0.01)
    res(2) shouldBe (5.309 +- 0.01)
  }
  
}