import scala.io.Source

object question1 extends App {
  def toInt(in: String): Int = in.toInt
  val input = Source.fromFile("/Users/christina/hub/advent2019/input.txt").getLines.map(toInt)

  def getFuelMass(mass: Int): Int = {
    val fuelMass = mass.toDouble./(3).floor.toInt.-(2)
    if (fuelMass >= 0)
      getFuelMass(fuelMass) + fuelMass
     else
      0
  }

  println(input.map(getFuelMass).sum)

}
