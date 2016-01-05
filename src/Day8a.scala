

import org.apache.logging.log4j.LogManager
import scala.io.Source._

object Day8a {

  val log = LogManager.getLogger("org.github.adventofcode")

  val expands = List( """\""", """"""" )

  def main( args : Array[String] ) : Unit = {

    log.info("day8")

    var lines = fromFile("./src/day8.txt").getLines.toList

    lines.foreach( Console.println(_) )

    val lengths = lines.map( _.size )

    lengths.foreach( Console.println(_) )

    expands.foreach( Console.println(_) )

    val expanded = lines.map( expand(_) )

    expanded.foreach( Console.println(_) )

    val dlengths = expanded.map( _.size )

    val zipped = lengths.zip(dlengths)

    val diffs = zipped.map( (p) => { p._2 - p._1 } )

    diffs.foreach( Console.println(_) )

    Console.println( diffs.foldLeft(0)( (accum,d) => { accum + d } ) )

  }

  def expand( src : String ) : String = {

    var cp = "\"" 
    
    for( c <- src ){
      
      c match {
        case '\"' => {
          cp += "\\\"" 
        }
        case '\\' => {
          cp += "\\\\" 
        }
        case _ => {
          cp += c
        }
      }
    }

    cp += "\""
    
    cp

  }

}