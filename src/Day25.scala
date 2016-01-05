

import scala.collection.mutable.ListBuffer
import org.apache.logging.log4j.LogManager

object Day25 {
  
  // To continue, please consult the code grid in the manual.  Enter the code at row 2978, column 3083.
  /*
   * 2978,3083
   * 2978
   * 3082
   * 5060,1
   */
  
  val log = LogManager.getLogger("org.github.adventofcode")

  def main( args : Array[String] ) = {
    
    log.info("start")
    
    var found = false
    
    var listPos = 0
    var diag = 1
    
    while( !found ){

      // loop over the diag
      val rs = ListBuffer[Int]()
      for( r <- diag to 1 by -1 ){
        rs += r
      }
      val cs = rs.reverse
      val zipped = rs.zip(cs)
      
      diag = diag + 1
      
      if( zipped.indexOf((2978,3083)) > 0 ){
        listPos = listPos + zipped.indexOf((2978,3083)) 
        found = true
      }
      else {
        listPos = listPos + zipped.size 
      }
      
    }
     
    var lastVal = BigInt(20151125)
    for( i <- 1 to listPos){
      val m1 = lastVal * BigInt(252533)
      lastVal = m1 % 33554393  
    }
     
    log.info("done")
    log.info( lastVal )
    log.info( lastVal == 2650453 )

  }
  
}