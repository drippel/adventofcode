

import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object Day14 {
  
  case class Deer( var name : String, var speed : Int, var runTime : Int, var restTime : Int )
  
  case class DeerState( var deer : Deer, var distance : Int = 0, var state : Int = 0, var score : Int = 0 )
  
  val log = LogManager.getLogger("org.github.adventofcode")
  
  def main( args : Array[String] ) : Unit = {
    
    log.info("Day14")

    var lines = fromFile("./src/day14.txt").getLines.toList
    
    // lines.foreach( Console.println(_) )
    val deers = lines.map( parseLine(_) )
    
    val states = deers.map( (d) => { DeerState( d, 0, d.runTime ) } )
    
    log.info(deers)

    Console.println( states(0) )
    
    // lets race
    for( i <- 1 to 2503 ){
      
      states.foreach( updateDeer( _, i ) ) 
      
      scoreDeer( states )
      
      Console.println( states(0) )
      
    }

    Console.println( states(0) )
    
    var distances = states.map( (d) => { d.distance } )
    distances = distances.sorted
    Console.println( distances )

    var scores = states.map( (d) => { d.score } )
    scores = scores.sorted
    Console.println( scores )
  }

  
  def parseLine( line : String ) : Deer = {
    val parts = line.split(' ')
    Deer( parts(0), parts(3).toInt, parts(6).toInt, parts(13).toInt )  
  }

  def updateDeer( state: DeerState, time: Int) = {
    
    if( state.state > 0 ){
      // running
      
      state.distance = state.distance + state.deer.speed
      
      state.state = state.state - 1
      
      if( state.state == 0 ){
        state.state = ( 0 - state.deer.restTime )
      }
    }
    else {
      // resting
      state.state = state.state + 1
      if( state.state == 0 ){
        state.state = state.deer.runTime
      }
    }
    
    

  }

  def scoreDeer(states: List[DeerState]) = {
    
    // sort the deer states by distances
    var distances = states.map( (d) => { d.distance } )
    distances = distances.sorted
    val max = distances.last 
    
    for( ds <- states ){
      if( ds.distance == max ){
        ds.score = ds.score + 1
      }
    }

  }

}