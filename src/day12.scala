

import org.apache.logging.log4j.LogManager
import scala.io.Source._

object day12 {
  
  val log = LogManager.getLogger("org.github.adventofcode")

  def main( args : Array[String] ) : Unit = {
    
    log.info("Day12")
    log.info( safeToInt( "-9") )

    var lines = fromFile("./src/day12a.json").getLines.toList
    
    val total = lines.foldLeft(0)( (accum,line) => {
      accum + sumLine(line)
    })
    
    log.info(total)
    log.info( safeToInt( "-9") )
  }
  
  def sumLine( line : String ) : Int = {
    
    val parts = line.split(' ').toList
    
    parts.foldLeft(0)( (accum,part) => {
      if( part.contains(',') ){
        val p2 = part.split(',')
        p2.foldLeft(accum)( (a2,p) => {
          a2 + safeToInt(p)
        })
      }
      else {
        accum + safeToInt(part)
      }
    })
    
  }
  
  def safeToInt( src : String ) : Int = {
    
    try{
     src.toInt
    }
    catch {
      case e : Exception => {
        0
      }
    }
    
  }
}