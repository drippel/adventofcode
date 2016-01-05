

import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.HashMap

object Day9 {
  
  val log = LogManager.getLogger("org.github.adventofcode")

  val siteMap = new HashMap[(String,String),Int]() 

  def main( args : Array[String] ) : Unit = {

    log.info("day9")

    var lines = fromFile("./src/day9.txt").getLines.toList
    
    var routes = lines.map( parse(_) )
    
    var sitePairs = routes.map( (t) => { List( t._1, t._2 ) } )
    

    for( r <- routes ) {
      siteMap.put( (r._1,r._2), r._3 )
    } 

    var paths = sitePairs.flatten.toSet.toList
    
    var distances = paths.permutations.map( (p) => { calcDistance(p)  }).flatten.toList.sorted
    
    Console.println(distances.last)

  }
  
  def parse( src : String ) : (String,String,Int) = {
    val parts = src.split(' ')
    ( parts(0),parts(2),parts(4).toInt)
  }
  
  def calcDistance( path : List[String] ) : Option[Int] = {
    
    var sum = 0
    
    for( i <- 0 until path.size - 1 ){
      
      findDistance(path(i),path(i+1)) match {
        case Some(d) => {
          sum = sum + d
        }
        case None => {
          return None
        }
      }
    }
    
    Some(sum)
  }
  
  def findDistance( from : String, to : String ) : Option[Int] = {
    
    siteMap.get( (from,to) ) match {
      
      case Some(d) => { Some(d) }
      case _ => {
        siteMap.get( (to,from) ) match {
          case Some(d) => { Some(d) }
          case _ => { None }
        }
      }
    }
  }

}