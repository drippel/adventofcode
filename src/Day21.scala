

import org.apache.logging.log4j.LogManager

object Day21 {
  
  case class Player( var hp : Int =0, var damage : Int = 0, var armor : Int = 0 )

  def getBoss() = { Player( 103, 9, 2 ) }
  
  case class Weapon( cost : Int, damage : Int )
  val dagger = Weapon( 8, 4 )
  val shortsword = Weapon( 10, 5 )
  val warhammer = Weapon( 25, 6 )
  val longsword = Weapon( 40, 7 )
  val greataxe = Weapon( 74, 8 )
  
  val weapons = List(dagger,shortsword,warhammer,longsword,greataxe)

  case class Armor( cost : Int, armor : Int )
  val leather = Armor( 13, 1 )
  val chainmail = Armor( 31, 2 )
  val splintmail = Armor( 53, 3 )
  val bandedmail = Armor( 75, 4 )
  val platemail = Armor( 102, 5 )
  
  val armors = List( Some(leather), Some(chainmail), Some(splintmail), Some(bandedmail), Some(platemail), None )

  case class Ring( cost : Int, damage : Int, armor : Int )
  val da1 = Ring( 25,  1, 0 )
  val da2 = Ring( 50,  2, 0 )
  val da3 = Ring( 100, 3, 0 )
  val df1 = Ring( 20, 0, 1 )
  val df2 = Ring( 40, 0, 2 )
  val df3 = Ring( 80, 0, 3 ) 
  val noring = Ring( 0, 0, 0 ) 
  
  val rings = List(Some(da1),Some(da2),Some(da3),Some(df1),Some(df2),Some(df3),None)
  
  def validKits() = { genKits().filter( validKit( _ ) ) }
  
  def genKits() = {
    
    for( w <- weapons;
         a <- armors;
         r1 <- rings;
         r2 <- rings )
      yield Kit(w,a,r1,r2)
    
  }
  
  def validKit( kit : Kit ) : Boolean = {
    
    kit.ring1 match {
      case Some(r1) => {
        
        kit.ring2 match {
          case Some(r2) => { r1 != r2 }
          case None => { true }
        }
      }
      case None => { true }
    }
  }
  
   
  val log = LogManager.getLogger("org.github.adventofcode")

  case class Kit( weapon : Weapon, armor : Option[Armor], ring1 : Option[Ring], ring2 : Option[Ring] )
  
  def main( args : Array[String] ) : Unit = {
    log.info("day21")
    
    val winners = for( 
     k <- validKits();
     if( !fight( calcPlayer(k), getBoss() ) ) ) yield k
     
    var costs = winners.map( (w) => { calcCost(w) } ) 
    
    costs = costs.sorted.reverse
    
    log.info( costs )
    
    
  }
  
  def selectItems() : Kit = {
    Kit(greataxe,Some(platemail),Some(da3),Some(df3))
  }

  def calcPlayer( kit : Kit ) = {
    
    var damage = kit.weapon.damage
    var armor = kit.armor match {
      case Some(a) => { a.armor }
      case None => { 0 }
    }
    
    kit.ring1 match {
      case Some(r) => {
        damage = damage + r.damage
        armor = armor + r.armor
      }
      case None => {
        //
      }
    }
    
    kit.ring2 match {
      case Some(r) => {
        damage = damage + r.damage
        armor = armor + r.armor
      }
      case None => {
        //
      }
    }
    
    Player(100,damage,armor) 
  }

  def fight( player: Player, boss : Player) : Boolean = {
    
    var winner = 0
    
    while( winner == 0 ){
      
      // player attacks
      var pd = player.damage - boss.armor 
      if( pd < 0 ) { pd = 1 } 
      
      boss.hp = boss.hp - pd
      
      if( boss.hp <= 0 ){
        winner = 1
      }
      else{

        // boss attacks
        var bd = boss.damage - player.armor
        if( bd <= 0 ){ bd = 1 }
        
        player.hp = player.hp - bd
        
        if( player.hp <= 0 ){
          winner = -1
        }
      }
      
    }
    
    winner > 0
    
  }

  def calcCost(kit: Kit) = {
    var cost = 0
    
    cost = cost + kit.weapon.cost
    
    val acost = kit.armor match {
      case Some(a) => { a.cost }
      case None => { 0 }
    }
    
    cost = cost + acost 
    
    val r1 = kit.ring1 match {
      case Some(a) => { a.cost }
      case None => { 0 }
    }

    cost = cost + r1 

    val r2 = kit.ring2 match {
      case Some(a) => { a.cost }
      case None => { 0 }
    }

    cost = cost + r2 

    cost
  }
}