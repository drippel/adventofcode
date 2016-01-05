

import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object Day13 {
  
  val log = LogManager.getLogger("org.github.adventofcode")
  
  val weights = new HashMap[(String,String),Int]()
  
  val names = new HashSet[String]()

  def main( args : Array[String] ) : Unit = {
    
    log.info("Day13")

    var lines = fromFile("./src/day13.txt").getLines.toList
    
    // lines.foreach( Console.println(_) )
    lines.foreach( parseLine(_) )

    log.info( weights )
    
    val combos = names.toList.permutations.toList
    
    var happys = combos.map( (c) => { calcHappiness(c) } )
    
    happys = happys.sorted
    
    happys.foreach( log.info( _ ) ) 
    
  }

  
  def parseLine( line : String ) = {
    
    val parts = line.split(' ')
    
    val t = (parts(0),parts(10).init)
    
    names += parts(0)
    names += parts(10).init
    
    val w = parts(2) match {
      case "gain" => { parts(3).toInt }
      case "lose" => { parts(3).toInt * -1 }
      case _ => { -1000000 }
    }
    
    weights += t -> w

    
  }

  def calcHappiness(c: List[String]) = {
    
    // for each pair get the happiness from the map
    var happy = 0
    
    for( i <- 0 to c.size -2 ){
      
      val t1 = (c(i),c(i+1))
      happy = happy + weights.get(t1).get
      
      val t2 = (c(i+1),c(i))
      happy = happy + weights.get(t2).get

    }
    
    val l1 = (c(c.size-1),c(0))
    happy = happy + weights.get(l1).get
    
    val l2 = (c(0),c(c.size-1))
    happy = happy + weights.get(l2).get

    happy

    
  }
  
}