
import scala.io.Source._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object DayThree {

  val north = (0,1)
  val south = (0,-1)
  val east = (1,0)
  val west = (-1,0)


  def main( args : Array[String] ) : Unit = {

    Console.println("day3")

    val lines = fromFile("./src/day3.txt").getLines.toList
    val path = lines(0)

    val moves = path.map( (c) => {
      c match {
        case '^' => north
        case 'v' => south
        case '>' => east
        case '<' => west
      }
    })

    Console.println( moves.size )

    val movePairs = moves.grouped(2).toList

    Console.println( movePairs.size )

    val start = List( ((0,0), (0,0)) )

    val houses = movePairs.foldLeft(start)( (accum,movePair) => {

      val (santa,robot) = accum.last
      val santaMove = movePair(0)
      val robotMove = movePair(1)

      val nextSanta = (santa._1 + santaMove._1, santa._2 + santaMove._2)
      val nextRobot = (robot._1 + robotMove._1, robot._2 + robotMove._2)

      val next = (nextSanta,nextRobot)

      accum ++ List(next)
    })

    val flatHouses = ListBuffer[(Int,Int)]()

    for( house <- houses ){
      flatHouses += house._1
      flatHouses += house._2
    }

    val unique = flatHouses.toSet

    // Console.println( houses )
    Console.println( unique.size )

  }

}