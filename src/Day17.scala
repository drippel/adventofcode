
import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.BitSet

object Day17 {

  val log = LogManager.getLogger("org.github.adventofcode")
  
  val containers = List( 50, 44, 11, 49, 42, 46, 18, 32, 26, 40, 21, 7, 18, 43, 10, 47, 36, 24, 22, 40 )
  val combos = makeCombos()

  def main( args : Array[String] ) : Unit = {
    
    log.info("day17")
    
    log.info(combos.size)
    
    val fills = findCombos()
    
    log.info(fills.size)
    
    var counts = fills.map( (c) => { countContainers(c) } )
    counts = counts.sorted
    
    val min = counts.head
    
    val minCombos = counts.filter( (c) => { c == min } )

    log.info(minCombos.size)
  }
  
  def makeCombos() = {
    
    val list = for( i <- 0 until ( 1048576 - 1 ) )
      yield i.toBinaryString.reverse.padTo(20, '0').reverse 
      
    list.toList
  }
  
  def findCombos() : List[String] = {
    for( combo <- combos; if( calcCombo(combo) == 150 ) )  yield combo
  }

  def calcCombo(combo: String) = {
    
    var sum = 0
    
    for( i <- 0 until 20 ){
      
      val a = combo(i).toString.toInt
      val b = containers(i) 
      
      sum = sum + ( a * b )
      
    }
    
    sum 
  }
  
  def countContainers( combo : String ) : Int = {
    
    var count = 0
    
    for( i <- 0 until 20 ){
      count = count + combo(i).toString.toInt
    }
    
    count
    
  }
  
}