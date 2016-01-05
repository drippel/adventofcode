
import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.ListBuffer

object Day24 {

  val log = LogManager.getLogger("org.github.adventofcode")
  
  class Package( var weight : Int )
  class Group( packages : List[Package], arrangement : List[Int] )

  var allPackages = List[Package]() 
  
  def main( args : Array[String] ) : Unit = {
    log.info("Day24")
    val lines = fromFile("./src/day24.txt").getLines.toList
    
    allPackages = lines.map( makePackage(_) )
    log.info( allPackages )

    val total = allPackages.foldLeft(0)( (accum,p) => { accum + p.weight } )
    log.info( total )
    
    val target = total / 4 
    log.info( target )
    
    val winner = makeCombos(target)

    log.info( winner )
    log.info( calcEntanglement(winner) )
    
  }
  
  def makePackage( line : String ) : Package = { new Package(line.toInt) }

  def makeCombos( target : Int ) = {

    var winner : String = "" 

    for( i <- 0 until ( 268435456 - 1 ) ){
      val bits = i.toBinaryString.reverse.padTo(28, '0').reverse 
      val w = calcWeight(bits)
      if( w == target ){
        
        if( winner.isEmpty ){
          winner = bits
        }
        else {
          
          val c1 = calcPackages(bits)
          val noOfPackages = calcPackages(winner)
          
          if( c1 < noOfPackages ){
            // we have a new leader 
            winner = bits
          }
          else if( c1 == noOfPackages ){
            // tie on packages 
            // calc entanglements
            val e1 = calcEntanglement(winner)
            val e2 = calcEntanglement(bits)
            if( e2 < e1 ){
              log.info("new winner")
              winner = bits
            }
          }
          else {
            // too many packages
          }
        }
      }
    }
    
    
    winner 
  }
  
  def calcWeight( bits : String ) : Int = {
    
    var sum = 0
    
    for( i <- 0 until bits.size ){
      val w = bits(i) match {
        case '0' => { 0 }
        case '1' => { allPackages(i).weight }
      }
      sum = sum + w
    }

    sum

  }
  
  def calcPackages( bits : String ) : Int = {
    
    var sum = 0
    
    for( i <- 0 until bits.size ){
      val w = bits(i) match {
        case '0' => { 0 }
        case '1' => { 1 } 
      }
      sum = sum + w
    }
    sum
  }

  def calcEntanglement( bits : String ) : BigInt = {
    
    var amounts = for( i <- 0 until bits.size ) yield {
      bits(i) match {
        case '0' => { 1 }
        case '1' => { allPackages(i).weight }
      }
    }
      
    amounts.foldLeft(BigInt(1))( (accum,a) => { accum * BigInt(a) } )  

  }

  def balance( g1 : List[Package], g2 : List[Package], g3 : List[Package] ) : Boolean = {
    true
  }
  
  def group1Small( g1 : List[Package], g2 : List[Package], g3 : List[Package] ) : Boolean = {
    true
  }
  
  def calcEntanglement( group : List[Package] ) : Int = {
    0
  }
  
  
  
}