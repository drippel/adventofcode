

import org.apache.logging.log4j.LogManager
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
object Day20 {

  val log = LogManager.getLogger("org.github.adventofcode")

  var rabbitCounts = HashMap[Int,Int]()  

  def main(args: Array[String]): Unit = {

    log.info("day20")
    
    val target = 33100000

    var i = 1
    var found = false
    
    while( i <= 5000000 && !found ){
      
      val fs = factors(i)
      val ps = presents(fs)
      
      Console.println(i + " " + ps )
      
      if( ps >= target ){
        Console.println( i )
        found = true
      }
      
      i = i + 1
    }
    
    log.info(i)
  }

  val factorMap = new HashMap[Int, List[Int]]()

  def factors(target: Int): List[Int] = {

    var factors = new HashSet[Int]()

    // 1 is always a factor
    factors += 1

    // calc
    for (i <- (target / 2) to 2 by -1) {

      if (target % i == 0) {
        if (!factors.contains(i)) {
          factorMap.get(i) match {
            case Some(fs) => {
              
              // log.info("in map:" + i)

              factors ++= fs

              for (f <- fs) {
                if (target % f == 0) {
                  factors += (target / f)
                }
              }
            }
            case None => {
              // 
              factors += i
            }
          }
        }
      }
    }

    // a number is always a factor of itself
    factors += target

    val factorList = factors.toList.sorted

    if (!factorMap.contains(target)) {
      factorMap += target -> factorList
    }

    factorList

  }

  def presents(nums: List[Int]): Int = {
    nums.foldLeft(0)((accum, n) => {
      val sum = accum + (n * 11 * rabbitDone(n) )
      incrRabbit(n)
      sum
    })
  }
  
  def rabbitDone( rabbit : Int ) : Int = {
    
    rabbitCounts.get(rabbit) match {
      case Some(c) => {
        if( c >= 50 ){
          0
        }
        else {
          1
        }
      }
      case None => { 1 }
    }
    
  }

  def factors2(target: Int): List[Int] = {

    var factors = new HashSet[Int]()

    // 1 is always a factor
    factors += 1

    // calc
    for (i <- (target / 2) to 2 by -1) {
      if (target % i == 0) {
        factors += i
      }
    }

    // a number is always a factor of itself
    factors += target

    val factorList = factors.toList.sorted

    factorList

  }

  def incrRabbit(n: Int) = {
    
    rabbitCounts.get(n) match {
      case Some(c) => {
        rabbitCounts.put(n,c+1)
      }
      case None => {
        rabbitCounts += n -> 1
      }
    }

  }
}