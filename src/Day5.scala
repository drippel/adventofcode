
import scala.io.Source._

object Day5 {

  val vowels = "aeiou"

  def main( args : Array[String] ) : Unit = {

    Console.println( vowels.contains('o') )

    val lines = fromFile("./src/day5.txt").getLines.toList

    Console.println("Lines:"+ lines.size)

    val meets1 = lines.filter( repeatingPair(_) )
    Console.println( meets1.size )

    val meets2 = meets1.filter( skipPair(_) )
    Console.println( meets2.size )

    meets2.foreach( Console.println(_) )
  }

  def vowelCount( src : String ) : Boolean = {
    // must contain three vowels
    val vs = src.filter( vowels.contains(_) )
    vs.size >= 3
  }

  def rule2( src : String ) : Boolean = {

    var found = false

    for( i <- 0 until src.size ) {

      val c1 = src.charAt(i)
      src.lift(i + 1) match {
        case Some(c2) => {
          if( c1.equals(c2) ){
            return true
          }
        }
        case None => {
          // nada
        }
      }
    }

    false
  }

  val bad = List( "ab", "cd", "pq", "xy" )

  def rule3( src : String ) : Boolean = {

    bad.find( src.contains(_) ) match {

      case Some(s) => {
        false
      }
      case None => {
        true
      }
    }

  }

  def repeatingPair( src : String ) : Boolean = {

    for( i <- 0 until src.size - 1 ){

      val c1 = src(i)
      val c2 = src(i+1)
      val pair = c1.toString() + c2

      val remainder = src.substring(i+2)

      if( remainder.contains(pair) ){
        Console.println( pair +" " + src )
        return true
      }

    }

    false
  }

  def skipPair( src : String ) : Boolean = {

    for( i <- 0 until src.size ){

      val c1 = src(i)
      src.lift(i+2) match {
        case Some(c2) => {

          if( c1.equals(c2) ){
            return true
          }
        }
        case _ => {
          // nada
        }
      }
    }

    false

  }
}