
import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.HashSet

object Day19 {
  
  val log = LogManager.getLogger("org.github.adventofcode")
  
  def main( args : Array[String] ) : Unit = {
    
    log.info("day19")
    
    val lines = fromFile("./src/day19a.txt").getLines.toList

    val start = fromFile("./src/day19b1.txt").getLines.toList.head
    
    val molecules = HashSet[String]()
    
    var replacements = lines.map( parseLine(_) )
    
    replacements = replacements.sortBy( (t) => t._2.size )
    replacements = replacements.reverse
    
    log.info( replacements )
    
    for( r <- replacements ){
      molecules ++= replaceMolecule( r._1, r._2, start )
    }
    
    log.info( molecules.size )
    
    val result = reduce( start, replacements )  
    log.info( result )
    
  }

  def parseLine( line : String) = {
    val parts = line.split(' ') 
    (parts(0),parts(2))
  }
  
  def replaceMolecule( from : String, to : String, start : String ) : Set[String] = {
    
    val molecules = HashSet[String]()
    
    var pos = start.indexOf(from)
    while( pos > -1 ){
      
      var newmole = start.substring(0, pos )
      newmole += to 
      newmole += start.substring( pos + from.size )  
      molecules += newmole
      pos = start.indexOf(from, (pos + from.size))
    }
    
    molecules.toSet
  }
  
  def reduce( start : String, replacements : List[(String,String)] ) : (String,Int) = {
    
    var done = false
    
    var count = 0 
    var current = start
    var pos = 0 
    
    while( pos < replacements.size ) {
      
      
      val result = reduce( current, replacements(pos) )
      if( result._2 > 0 ){
        log.info( "reduced "+ replacements(pos) + " " + result._2 )
        log.info( result._1 )
        // replaced something
        count = count + result._2
        current = result._1
        pos = 0
      }
      else {
        // no replacements
        pos = pos + 1
        
      }
      
    }
    

    (current,count)
  }

  def reduce( start : String, t : (String,String) ) : (String,Int) = {
    
    var dest = start
    var count = 0
    
    var pos = dest.indexOf(t._2) 
    while( pos > -1){
      dest = dest.substring(0, pos) + t._1 + dest.substring(pos+t._2.size)
      pos = dest.indexOf(t._2) 
      count = count + 1
    }
    
    (dest,count)
    
  }
  
  

}