

import org.apache.logging.log4j.LogManager
import scala.io.Source._

object Day10 {
  
  val log = LogManager.getLogger("org.github.adventofcode")

  def main( args : Array[String] ) : Unit = {
    
    log.info("day10")
    
    var start = "1113122113"
    log.info(start)
    
    for( i <- 0 until 50 ){
      
      start = makeCounts(start)
      log.info( i +" "+ start.size +" "+ start)
      
    }
    
    // log.info(start)
    
  }
  
  def makeCounts( curr : String ) : String = {


    val output = new StringBuilder("")
    var count = 0
    
    // 311311222113 
    
    for( i <- 0 until curr.size ){
      
      val c1 = curr(i)
      
      curr.lift(i-1) match {
        case Some(c2) => {
          
          if( c1 != c2 ){
            output.append( count.toString + c2.toString ) 
            count = 1
          }
          else {
            count = count + 1
          }
          
        }
        case _ => {
          // start of string
          count = 1
        }
      }
      
      // are we at the end
      if( i == (curr.size - 1) ){
        output.append( count.toString + c1.toString ) 
      }
      
    }
    
    // log.info(output)
    
    output.toString()
    
  }
  
}