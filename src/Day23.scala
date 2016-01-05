
import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.ListBuffer

object Day23 {
  
  val log = LogManager.getLogger("org.github.adventofcode")
  
  case class Memory( var a : BigInt = 0, var b : BigInt = 0 )
  
  val globalMemory = Memory(1,0)
  
  class Instruction
  case class Half( r : String ) extends Instruction
  case class Triple( r : String ) extends Instruction
  case class Increment( r : String) extends Instruction
  case class Jump( i : Int) extends Instruction
  case class JumpIfEven( r : String, i : Int) extends Instruction
  case class JumpIfOne( r : String, i : Int) extends Instruction

  def main( args : Array[String] ) : Unit = {
    log.info("Day23")
    
    // parse program
    val lines = fromFile("./src/day23.txt").getLines.toList
    
    val instructions = parseInstructions(lines)
    
    // execute program

    executeInstructions( instructions)
    
    log.info( "Memory:" + globalMemory.a +" "+ globalMemory.b )
  }

  def parseInstructions(lines: List[String]) : List[Instruction] = {
    lines.map( parseInstruction(_) )
  }
  
  def parseInstruction( line : String ) : Instruction = {
    
    val parts = line.split(' ')
    
    parts(0).trim() match {
      case "hlf" => { Half( parts(1) ) }
      case "tpl" => { Triple( parts(1) ) }  
      case "inc" => { Increment(parts(1)) }  
      case "jmp" => { Jump( parts(1).toInt ) }
      case "jie" => { JumpIfEven( parts(1), parts(2).toInt ) } 
      case "jio" => { JumpIfOne( parts(1), parts(2).toInt ) } 
      case _ => {
        log.info( parts(0) )
        throw new IllegalStateException( "unknown instruction:" + parts(0) )
      }
    }
    
  }

  def executeInstructions( instructions : List[Instruction] ) = {

    var pos = 0
    
    while( pos < instructions.size ){
      
      log.info("pos:" + pos )
      log.info( "Memory:" + globalMemory.a +" "+ globalMemory.b )

      instructions(pos) match {
        case h : Half => {
          log.info("half")
          if( h.r == "a" ){
            globalMemory.a = ( globalMemory.a / 2 )
          }
          else {
            globalMemory.b = ( globalMemory.b / 2 )
          }
          pos = pos + 1
        }
        case t : Triple => {
          log.info("triple")
          if( t.r == "a" ){
            globalMemory.a = ( globalMemory.a * 3 )
          }
          else {
            globalMemory.b = ( globalMemory.b * 3 )
          }
          pos = pos + 1
        } 
        case i : Increment => {
          log.info("increment")
          if( i.r == "a" ){
            globalMemory.a = globalMemory.a + 1
          }
          else {
            globalMemory.b = globalMemory.b + 1
          }
          pos = pos + 1
        }
        case j : Jump => {
          log.info("jump")
          pos = pos + j.i
        }
        case jie : JumpIfEven => {
          log.info("even")

          val value = if( jie.r.startsWith("a") ){ globalMemory.a }
          else { globalMemory.b }

          val offset = if( value % 2 == 0 ){ jie.i }
          else { 1 }

          pos = pos + offset
        } 
        case jio : JumpIfOne => {
          log.info("one")
          val value = if( jio.r.startsWith("a") ){ globalMemory.a }
          else { globalMemory.b }

          val offset = if( value == 1 ){ jio.i }
          else { 1 }

          pos = pos + offset
        } 
      }

      log.info( "Memory:" + globalMemory.a +" "+ globalMemory.b )
    }

  }
  
}