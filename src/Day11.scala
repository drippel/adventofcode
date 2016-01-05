
import org.apache.logging.log4j.LogManager
import scala.io.Source._
import scala.collection.mutable.HashSet

object Day11 {
  
  val log = LogManager.getLogger("org.github.adventofcode")

  def main( args : Array[String] ) : Unit = {
    log.info( "Day11" )
    
    // Console.println(nextChar('d'))
    // Console.println(nextChar('z'))
    
    // val currPassword = "vzbxkghb"
    // val currPassword = "vzbxkghz"
    // val currPassword = "vzbxkzzz"
    val currPassword = "vzbxxyzz"
    Console.println(currPassword)
    Console.println(findNextPassword(currPassword))
    
  }
  
  def findNextPassword( start : String ) : String = {
    
    var found = false
    var pwd = start

    while( !found ){
      
      Console.println( "nope:" + pwd )

      pwd = nextPassword(pwd)
      if( isValidPassword(pwd) ) {
        return pwd
      }
    }
    
    "wtf"
  }
  
  def nextPassword( start : String ) : String = {
    
    var pos = start.size - 1
    
    nextChar(start(pos)) match {
      case Some(c) => {
        start.substring(0,pos) + c.toString() + start.substring(pos+1)
      }
      case None => {
        // reached a z 
        // set pos = a
        // increment the one to the left
        // + start(pos+2)
        var from = rollFrom(start)
        val c = nextChar(start(from-1))
        start.substring(0,from-1) + c.get + fill(start.size - from ) 
      }
    }
    
  }
  
  def fill( size : Int ) : String = {
        var end = ""
        for( i <- 0 until size ){
          end = end + 'a'
        }
        end
  }
  
  def rollFrom( src : String ) : Int = {
    
    var pos = src.size - 1
    
    for( i <- src.size - 1 to 0 by -1 ){
      
      if( src(i) == 'z' ){
        pos = i
      }
      else {
        return pos
      }
      
    }
    
    pos
    

    
  }
  
  
  def nextChar( c : Char ) : Option[Char] = {
    
    if( c < 'z' ){
      Some((c+1).toChar)
    }
    else {
      None
    }
  }
  
  def isValidPassword( src : String ) : Boolean = {
    
    rule1(src) && rule2(src) && rule3( src )
    
  }
  
  def rule1( src : String ) : Boolean = {
    
    var len = 1
    
    for( i <- 0 until ( src.size - 2 ) ){
      
      val c1 = src(i)
      val c2 = src(i+1)
      val c3 = src(i+2)
      if( (c2 == (c1+1).toChar) && ( c3 == (c2+1).toChar) ){
        return true
      }
      
    }
    
    false
    
  }
  
  def rule2( src : String ) : Boolean = {
    
    if( src.contains("i") 
        || src.contains("l")
        || src.contains("o") ){
      false
    }
    else {
      true
    }
  }

  def rule3( src : String ) : Boolean = {
    
    val pairs = HashSet[Char]()
    
    var i = 0
    while( i < src.size - 1 ){
      
      val c1 = src(i)
      val c2 = src(i+1)
      
      if( c1.equals(c2) ){
        
        if( !pairs.contains(c1) ){
          pairs += c1 
          // move forward
          i = i + 2
        }
        else {
          i = i + 1
        }
      }
      else {
        i = i + 1
      }
      
    }
    
    pairs.size >= 2
    
  }
}