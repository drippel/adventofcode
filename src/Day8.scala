

import org.apache.logging.log4j.LogManager
import scala.io.Source._

object Day8 {

  val log = LogManager.getLogger("org.github.adventofcode")

  val escapes = List( """\\""", """\"""" )

  val hex = """\x"""

  def main( args : Array[String] ) : Unit = {

    log.info("day8")

    var lines = fromFile("./src/day8.txt").getLines.toList

    lines.foreach( Console.println(_) )

    val lengths = lines.map( _.size )

    lengths.foreach( Console.println(_) )

    escapes.foreach( Console.println(_) )

    val collapsed = lines.map( unescape(_) )

    val dehexed = collapsed.map( dehex(_) )
    dehexed.foreach( Console.println(_) )

    val dlengths = dehexed.map( _.size )

    val zipped = lengths.zip(dlengths)

    val diffs = zipped.map( (p) => { p._1 - (p._2 - 2 ) } )

    diffs.foreach( Console.println(_) )

    Console.println( diffs.foldLeft(0)( (accum,d) => { accum + d } ) )

  }

  def unescape( src : String ) : String = {

    escapes.foldLeft(src)( (accum,esc) => { unescape(accum,esc) } )
  }

  def unescape( src : String, esc : String ) : String = {

    var cp = src.toString()

    var pos = cp.indexOf(esc)

    while( pos > -1 ){

      val start = cp.substring(0, pos)
      val end = cp.substring(pos + 2)
      cp = start + "|" + end

      pos = cp.indexOf(esc)

    }

    cp

  }

  def dehex( src : String ) : String = {

    var cp = src.toString()

    var pos = cp.indexOf(hex)

    while( pos > -1 ){

      val start = cp.substring(0, pos)
      val end = cp.substring(pos + 4)
      cp = start + "|" + end

      pos = cp.indexOf(hex)

    }

    cp
  }

}