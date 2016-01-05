

import org.apache.logging.log4j.LogManager
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
object Day20a {

  val log = LogManager.getLogger("org.github.adventofcode")

  def main(args: Array[String]): Unit = {

    log.info("day20a")
    
    var houses = ListBuffer[Int](0)

    val target = 33100000
    
    for( i <- 0 to 3310001 ){
      houses += 0
    }
      
    
    for( rabbit <- 1 to (3310000) ){
      Console.println("Rabbit:"+ rabbit )
      val amt = rabbit * 10
      for( j <- rabbit to (3310000) by rabbit ){
        Console.println("House:"+ j )
        houses(j) = houses(j) + amt
        if( houses(j) == 33100000 ){
          Console.println( j )
          System.exit(0)
        }
      }
    }


  }

}