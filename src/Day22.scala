
import org.apache.logging.log4j.LogManager
import scala.collection.mutable.ListBuffer


object Day22 {
  
  val log = LogManager.getLogger("org.github.adventofcode")

  def main( args : Array[String] ) : Unit = {
    log.info("day22")
    
    val all = permutations( List('m','d','s','p','r'), 20 )
    // all.foreach( Console.println(_) )
    log.info("combos:"+ all.size )
    // all.foreach( Console.println(_) )
  }
  
  def permutations( elements : List[Char], length : Int ) : List[List[Char]] = {
    
    var allPerms = ListBuffer[List[Char]]()
    
    
    def inner1( accum : List[Char], es : List[Char] ) {
      
      // log.info(accum)
      
      if( accum.size >= length ){
        allPerms += accum
      }
      else {
        
        es match {
          case Nil => {
            // log.info("nil "+ accum)
          }
          case hd :: tail => {
            val na = accum :+ hd  
            inner1( na, elements )
            inner1( accum, tail )
          }
        }
      }
    }
      
    inner1( List[Char](), elements )
    
    allPerms.toList
    
  }
  
  case class Boss( var hp : Int = 0, var damage : Int =0, var armor : Int = 0 )
  case class Player( var hp : Int =0, var mana : Int = 0, var armor : Int = 0 )
  
  case class Spell( val cost : Int, val damage : Int, val armor : Int, val heal : Int, val duration : Int, val mana : Int )
  
  val missile = Spell( 53, 4, 0, 0, 0, 0 )
  val drain = Spell( 73, 2, 0, 2, 0, 0 )
  val shield = Spell( 113, 0, 7, 0, 6, 0 )
  val poison = Spell( 173, 3, 0, 0, 6, 0 )
  val recharge = Spell( 229, 0, 0, 0, 5, 101 )
  
  val spells = List( missile, drain, shield, poison, recharge )
  
  def makePlayer() : Player = { Player( 50, 500, 0 ) }
  
  def makeBoss() : Boss = { Boss( 58, 9, 0 ) }
  
  def genSpellLists() : List[Spell] = {
    
    // lets say no battles will exceed 25 rounds
    List[Spell]()
    
  }
  
  def isValidSpellList( list : List[Spell] ) : Boolean = {
   true 
  }
  
  class Result
  case class Win() extends Result
  case class Lose() extends Result
  case class Draw() extends Result

  
  def fight( player : Player, boss : Boss ) : Result = {
    Win()
   }

}