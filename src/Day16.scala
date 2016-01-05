
import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.HashMap

object Day16 {
  
  val log = LogManager.getLogger("org.github.adventofcode")
  
  case class Sue( var name : String, var facts : Map[String,Int] )

  def main( args : Array[String] ) : Unit = {
    
    log.info("day16")

    var lines = fromFile("./src/day16.txt").getLines.toList
    
    var sues = lines.map( parseLine(_) ) 
    
    sues.foreach( Console.println(_) )
    
    val facts = makeFacts()
    
    val found = sues.filter( (s) => { checkFacts(s) } )
    
    found.foreach( Console.println(_) )
    
    log.info( found.size )
    log.info( found(0) )
  }

  def parseLine(line: String) : Sue = {

    val p1 = line.indexOf(':')
    var nm = line.substring(0,p1).split(' ')(1)
    
    val factsRaw = line.substring(p1+1).split(',')
    
    val facts = HashMap[String,Int]()
    
    for( f <- factsRaw ){
      
      val parts = f.split(':')
      
      facts += parts(0).trim -> parts(1).trim.toInt
      
    }
    
    Sue(nm,facts.toMap)
  }
  
  def makeFacts() : Map[String,Int] = {

    val facts = HashMap[String,Int]()
    
    facts += "children" -> 3
    facts += "cats" ->  7
    facts += "samoyeds" -> 2
    facts += "pomeranians" -> 3
    facts += "akitas" -> 0
    facts += "vizslas" -> 0
    facts += "goldfish" -> 5
    facts += "trees" -> 3
    facts += "cars" -> 2
    facts += "perfumes" -> 1 

    facts.toMap
    
  }

  def checkFacts(s: Sue, facts: Map[String,Int]) : Boolean = {
    
    for( fact <- s.facts ){
      
      val f = facts.get(fact._1).get
      
      if( fact._2 != f ){
        return false
      }
      
    }
    
    true 
  }
  
  def checkFact( sue : Sue, key : String, value : Int, op : (Int,Int) => Boolean = eq ) : Boolean = {
    
    sue.facts.get( key ) match {
      case Some(i) => { 
        if( !op( i, value ) ){
          return false
        }
      }
      case None => {}
    }
    
    true
  }
  
  def eq( i1 : Int, i2 : Int ) : Boolean = {
    i1 == i2
  }
  
  def gtr( i1 : Int, i2 : Int ) : Boolean = {
    i1 > i2
  }
  
  def lsr( i1 : Int, i2 : Int ) : Boolean = {
    i1 < i2
  }

  def checkFacts( sue : Sue ) : Boolean = {

    if( !checkFact( sue, "children", 3 ) ) {
      return false
    }

    if( !checkFact( sue, "samoyeds", 2 ) ){
      return false
    }

    if( !checkFact( sue, "akitas", 0 ) ){
      return false
    }

    if( !checkFact( sue, "vizslas", 0 ) ){
      return false
    }


    if( !checkFact( sue, "cars", 2 ) ){
      return false
    }
    
    if( !checkFact( sue, "perfumes", 1 ) ){
      return false
    }
    
    if( !checkFact( sue, "cats", 7, gtr ) ){
      return false
    }

    if( !checkFact( sue, "trees", 3, gtr ) ){
      return false
    }

    if( !checkFact( sue, "pomeranians", 3, lsr ) ){
      return false
    }

    if( !checkFact( sue, "goldfish", 5, lsr ) ){
      return false
    }

    true
    
  }

  
}