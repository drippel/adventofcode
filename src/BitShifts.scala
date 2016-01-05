

object BitShifts {

  def main( args : Array[String] ) : Unit = {
    Console.println("test")

    val x : Char = 123
    val y : Char = 456

    val d = x.&(y)

    val n = ~x

    Console.println(d)
    Console.println(d.toBinaryString)
    Console.println(x.|(y))
    Console.println(x.<<(2))
    Console.println(y.>>(2))
    Console.println( (Char.MaxValue - x) )
  }



}