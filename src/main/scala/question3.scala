import scala.io.Source
import scala.math.abs

object question3 extends App {
  def toInt(in: String): Int = in.toInt
  val input = Source.fromFile("/Users/christina/hub/advent2019/input3.txt").getLines.toArray

  val wire1 = input(0).mkString.split(",").toList
  val wire2 = input(1).mkString.split(",").toList

  // function to convert wire lists to list of tuples
  def toTuple(wire: String) : (String, Int) = {
    val direction = wire.substring(0,1)
    val magnitude = wire.substring(1,wire.length).toInt
    (direction, magnitude)
  }

  def getWireCoords(dirList:List[(String,Int)]): List[(Int,Int)] = {
    // access the nth array
    var x = 0
    var y = 0

    val wireCoords = for (point <- dirList) yield {
      val x_ = point._1 match {
        case "L" => x - point._2
        case "R" => x + point._2
        case "U" => x
        case "D" => x
      }
      val y_ = point._1 match {
        case "L" => y
        case "R" => y
        case "U" => y + point._2
        case "D" => y - point._2
      }

      val coordinates = point._1 match {
        case "L" => {
          val xRange = (x until x_ by -1).toList
          for (c <- xRange) yield (c, y)
        }
        case "R" => {
          val xRange = (x until x_).toList
          for (c <- xRange) yield (c, y)
        }
        case "U" => {
          val yRange = (y until y_).toList
          for (c <- yRange) yield (x, c)
        }
        case "D" => {
          val yRange = (y until y_ by -1).toList
          for (c <- yRange) yield (x, c)
        }

      }
      x = x_
      y = y_

      coordinates
    }
    wireCoords.flatten
  }

  def getDistance(coordinates: (Int, Int)): Int = {
    math.abs(coordinates._1) + math.abs(coordinates._2)
  }

  def stepsToIntersection(wire1: List[(Int, Int)], wire2: List[(Int, Int)])(intersection: (Int,Int)): ((Int,Int),Int) = {
    // indexOf() returns the first index of
    (intersection,wire1.indexOf(intersection) + wire2.indexOf(intersection))
  }

  val dirList1 = wire1.map(toTuple)
  val dirList2 = wire2.map(toTuple)

  val wire1Coords = getWireCoords(dirList1).drop(1)
  val wire2Coords = getWireCoords(dirList2).drop(1)

  for (item <- wire1Coords) {
    println(item)
  }

  val commonCoords = wire1Coords.toSet.intersect(wire2Coords.toSet)
  val minDistance = commonCoords.map(getDistance).min

  println("The closest intersection is "+minDistance+" steps from the origin.")

  val intersectionDistances = commonCoords.toList.map(stepsToIntersection(wire1Coords, wire2Coords))
  val intersectionDistancesMap = intersectionDistances.groupBy(_._1).map { case (k,v) => (k,v.map(_._2)(0))}

  // add 2 to account for the origin and destination nodes
  println(intersectionDistancesMap.valuesIterator.min+2)

}
