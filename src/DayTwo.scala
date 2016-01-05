
import scala.io.Source._

object DayTwo {

  def main( args : Array[String] ) : Unit  = {

    Console.println("day2")

    // read in the data
    val lines = fromFile("./src/day2.txt").getLines.toList

    Console.println(lines.size)

    var total = 0

    var ribbon = 0

    for( line <- lines ){

      val parts = line.split('x')

      val dim1 = parts.map { _.toInt }
      val dim2 = dim1.toList
      val dim3 = dim2.sorted

      Console.println(dim3)

      val (h,l,w) = (dim3(0),dim3(1),dim3(2))

      val surface = (2 * ( h * l )) + (2 * ( h * w )) + (2 * ( l * w ))

      val extra = ( h * l )

      val needed = surface + extra

      total = total + needed

      val circ = ( 2 * h ) + ( 2 * l )
      val box = ( h * l * w )
      ribbon = ribbon + circ + box


    }

    Console.println(total)
    Console.println(ribbon)
  }
}