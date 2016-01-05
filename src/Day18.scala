

import org.apache.logging.log4j.LogManager
import scala.io.Source._

object Day18 {
  
  val log = LogManager.getLogger("org.github.adventofcode")

  
  val OFF = '.'
  val ON = '#'
  
  val directions = List( (-1,1), (0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0) )
  
  def main( args : Array[String] ) : Unit = {

    log.info("day18")
    val lines = fromFile("./src/day18.txt").getLines.toList

    var grid = Array.ofDim[Char](100, 100)
    
    for( y <- 0 until 100 ){
      for( x <- 0 until 100 ){
        grid(x)(y) = lines(y)(x)
      }
    }

    printGrid(grid)
    
    for( i <- 1 to 100 ){
      for( j <- 1 to 100 ){
        Console.print("-")
      }
      Console.print("\n")
      grid = liveAndDie(grid) 
      printGrid(grid)
    }

    Console.println(liveCount(grid))
  }
  
  def liveCount( grid : Array[Array[Char]] ) : Int = {
    val live = for( x <- 0 until 100; y <- 0 until 100; if( grid(x)(y) == ON ) ) yield grid(x)(y)
    live.size
  }
  

  def printGrid(grid: Array[Array[Char]]) = {
    for( y <- 0 until 100 ){
      for( x <- 0 until 100 ){
        Console.print(grid(x)(y))
      }
      Console.print("\n")
    }
  }
  
  def liveAndDie( grid : Array[Array[Char]] ) : Array[Array[Char]] = {
    var next = Array.ofDim[Char](100, 100)
    
    for( x <- 0 until 100 ){
      for( y <- 0 until 100 ){
        next(x)(y) = liveOrDie( grid, x, y )
        adjustCorners(next)
      }
    }
    
    next
  }

  def liveOrDie(grid: Array[Array[Char]], x: Int, y: Int) : Char = {
    
    val live = liveCount(getNeighbors( grid, x, y ))
    
    grid(x)(y) match {
      
      case ON => {
        if( live == 2 || live == 3 ){ ON }
        else { OFF }
      }
      case OFF => {
        if( live == 3 ){ ON }
        else{ OFF }
      }
    }
  }
  
  def liveCount( ns : List[Char] ) : Int = {
    ns.filter( (c) => { c == ON } ).size 
  }

  def getNeighbors(grid: Array[Array[Char]], x: Int, y: Int) : List[Char] = {
    
    val ns = directions.map( (dir) => {
      grid.lift((x+dir._1)) match {
        case Some(ys) => {
          ys.lift((y+dir._2)) match {
            case Some(c) => { Some(c) }
            case _ => { None }
          }
        }
        case _ => { None }
      }
    })
    
    ns.flatten
    
  }

  def adjustCorners(next: Array[Array[Char]]) = {
    next(0)(0) = ON
    next(0)(99) = ON
    next(99)(0) = ON
    next(99)(99) = ON
    
  }
  

}