
import scala.io.Source._

object Day6 {

  val grid = Array.ofDim[Int](1000, 1000)

  case class Instruction( intsr : String, c1 : (Int,Int), c2 : (Int,Int) )

  def main(args: Array[String]): Unit = {
    Console.println("day6")

    // initialize grid
    for (x <- 0 until 1000; y <- 0 until 1000) {
      grid(x)(y) = 0
    }

    printGrid()

    // make to instructions
    val lines = fromFile("./src/day6.txt").getLines.toList

    val instructions = lines.map( (line) => {

      val parts = line.split(' ')
      if( parts.size == 4 ){

        val i = "toggle"
        val c1 = parts(1).split(',')
        val c2 = parts(3).split(',')
        Instruction(i, (c1(0).toInt, c1(1).toInt), (c2(0).toInt, c2(1).toInt) )

      }
      else {
        val c1 = parts(2).split(',')
        val c2 = parts(4).split(',')
        Instruction(parts(1), (c1(0).toInt, c1(1).toInt), (c2(0).toInt, c2(1).toInt) )
      }

    })

    instructions.foreach( Console.println(_))

    val i1 = Instruction( "on", (800,800), (999,999) )

    // apply instructions
    instructions.foreach( applyInstruction( _ ) )

    printGrid()

    // count on light
    var on = 0
    for (x <- 0 until 1000; y <- 0 until 1000 ){
      on = on + grid(x)(y)
    }

    Console.println(on)
  }

  def printGrid() = {
    for (x <- 0 until 1000) {

      for (y <- 0 until 1000) {
        val c = if( grid(x)(y) > 0 ) { '*' }
        else { ' ' }
        Console.print(c)
      }
      Console.print("\n")
    }
  }

  def applyInstruction( instr : Instruction ) : Unit = {

    for( x <- instr.c1._1 to instr.c2._1 ){
      for( y <- instr.c1._2 to instr.c2._2 ){

        instr.intsr match {
          case "on" => { grid(x)(y) = grid(x)(y) + 1 }
          case "off" => {
            grid(x)(y) = grid(x)(y) - 1
            if( grid(x)(y) < 0 ){
              grid(x)(y) = 0
            }
          }
          case "toggle" => {grid(x)(y) = grid(x)(y) + 2 }
        }
      }
    }

  }

}