
import scala.io.Source._

object DayOne {

  def main(args: Array[String]): Unit = {

    // read in the data
    val lines = fromFile("./src/data.txt").getLines.toList

    Console.println("day1")
    Console.println(lines(0).size)

    var pos = 0
    var ups = 0
    var downs = 0
    var floor = 0

    for (c <- lines(0)) {
      pos = pos + 1
      c match {
        case '(' => {
          ups = ups + 1
          floor = floor + 1
        }
        case ')' => {
          downs = downs + 1
          floor = floor - 1
        }
        case _ => {}
      }

      if (floor < 0) {
        Console.println("basement:" + pos)
      }

    }

    Console.println(floor)

    val basement = lines(0).foldLeft((0, 0))((sum, c) => {

      if (sum._1 < 0) {
        sum
      } else {

        c match {
          case '(' => { (sum._1 + 1, sum._2 + 1) }
          case ')' => { (sum._1 - 1, sum._2 + 1) }
          case _ => sum

        }
      }
    })

    Console.println(basement)

  }
}
