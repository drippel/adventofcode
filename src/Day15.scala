

import org.apache.logging.log4j.LogManager
import scala.io.Source._

object Day15 {
  
  val log = LogManager.getLogger("org.github.adventofcode")
  
  case class Ingredient( var name : String, var capacity : Int = 0, var durability : Int = 0,
      var flavor : Int = 0, var texture : Int = 0, var calories : Int = 0 )
      
  val sprinkles = Ingredient( "Sprinkles", 2, 0, -2, 0, 3 )
  val butterscotch = Ingredient( "Butterscotch", 0, 5, -3, 0, 3 )
  val chocolate = Ingredient( "Chocolate", 0, 0, 5, -1, 8 )
  val candy = Ingredient( "Candy", 0, -1, 0, 5, 8 )
    
  val ingredients = List(sprinkles,butterscotch,chocolate,candy)
      
  def main( args : Array[String] ) : Unit = {
    log.info("day15")
    
    val possibleRecipes = createPossibleRecipes()
    // possibleRecipes.foreach( Console.println(_) )
    Console.println( possibleRecipes.size )
    
    var totals = evaluateRecipes(possibleRecipes)
    
    totals = totals.filter( (t) => { t._2 == 500 } )

    totals = totals.sorted
    totals.foreach( Console.println(_) )
    
    
  }

  def createPossibleRecipes() = {
    
    val amounts = (for( i <- 1 to 100 ) yield i).toList
    
    val combos = for( a <- 0 to 100; 
      b <- 0 to 100; 
      c <- 0 to 100; 
      d <- 0 to 100;
      if( (a + b + c + d) == 100 ) ) yield (a,b,c,d)
      
   combos.toList
    
    
    
  }

  def evaluateRecipes(possibleRecipes: List[(Int, Int, Int, Int)]) : List[(Int,Int)] = {
    
    possibleRecipes.map( (amounts) => {

      var capacity = (amounts._1 * ingredients(0).capacity) + (amounts._2 * ingredients(1).capacity) + (amounts._3 * ingredients(2).capacity) + (amounts._4 * ingredients(3).capacity)
      var durability = (amounts._1 * ingredients(0).durability) + (amounts._2 * ingredients(1).durability) + (amounts._3 * ingredients(2).durability) + (amounts._4 * ingredients(3).durability)
      var flavor = (amounts._1 * ingredients(0).flavor) + (amounts._2 * ingredients(1).flavor) + (amounts._3 * ingredients(2).flavor) + (amounts._4 * ingredients(3).flavor)
      var texture = (amounts._1 * ingredients(0).texture) + (amounts._2 * ingredients(1).texture) + (amounts._3 * ingredients(2).texture) + (amounts._4 * ingredients(3).texture)

      var calories = (amounts._1 * ingredients(0).calories) + (amounts._2 * ingredients(1).calories) + (amounts._3 * ingredients(2).calories) + (amounts._4 * ingredients(3).calories)
      
      capacity = if(capacity < 0 ) 0 else capacity 
      durability = if(durability < 0 ) 0 else durability 
      flavor = if(flavor < 0 ) 0 else flavor 
      texture = if(texture < 0 ) 0 else texture 
      calories = if(calories < 0 ) 0 else calories 

      (( capacity * durability * flavor * texture ),calories)
    })
    
    
  }
  
}