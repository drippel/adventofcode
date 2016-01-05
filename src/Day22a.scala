
import org.apache.logging.log4j.LogManager
import scala.collection.mutable.ListBuffer


object Day22a {
  
  val log = LogManager.getLogger("org.github.adventofcode")

  def main( args : Array[String] ) : Unit = {
    log.info("day22a")
    
    val initialState = new GameState( makeBoss, makePlayer, 0, 0, ListBuffer[Spell](), ListBuffer[SpellEffect]() )
    fight(initialState)
    
    log.info(winningGames)
    winningGames.foreach( (g) => { log.info( "spent:"+ g.manaSpent + "spells:" + g.spellsCast ) } )
    
  }
  
  case class Boss( var hp : Int = 0, var damage : Int =0, var armor : Int = 0 )
  case class Player( var hp : Int =0, var mana : Int = 0, var armor : Int = 0 )
  
  class Spell( val cost : Int, val damage : Int, val armor : Int, val heal : Int, val duration : Int, val mana : Int )
  
  case class Missile() extends Spell( 53, 4, 0, 0, 0, 0 )
  case class Drain() extends Spell( 73, 2, 0, 2, 0, 0 )
  case class Shield() extends Spell( 113, 0, 7, 0, 6, 0 )
  case class Poison() extends Spell( 173, 3, 0, 0, 6, 0 )
  case class Recharge() extends Spell( 229, 0, 0, 0, 5, 101 )
  
  case class SpellEffect( var spell : Spell, var remaining : Int )
  
  val allSpells = List( Missile(), Drain(), Shield(), Poison(), Recharge() )
  
  def makePlayer() : Player = { Player( 50, 500, 0 ) }
  def makeBoss() : Boss = { Boss( 58, 9, 0 ) }
  def makeDemoPlayer() : Player = { Player( 10, 250, 0 ) }
  def makeDemoBoss() : Boss = { Boss( 14, 8, 0 ) }
  
  class Result( var state : GameState )
  class Win( state : GameState ) extends Result(state)
  class Lose( state : GameState ) extends Result(state)
  
  class GameState( val boss : Boss, val player : Player, 
      var round : Int = 0, 
      var manaSpent : Int, 
      var spellsCast : ListBuffer[Spell],
      var effects : ListBuffer[SpellEffect] )
      
  def clone( state : GameState ) : GameState = {
    
    val effects = state.effects.map( (se) => { clone(se) } ) 
    
    new GameState( clone(state.boss), clone(state.player), 
        state.round, state.manaSpent, 
        state.spellsCast.clone(), 
        effects )
  }
  
  def clone( boss : Boss ) : Boss = { Boss(boss.hp, boss.damage, boss.armor) }
  
  def clone( player : Player ) : Player = { Player( player.hp, player.mana, player.armor ) }

  def clone( effect : SpellEffect ) : SpellEffect = { SpellEffect( effect.spell, effect.remaining ) }
  
  val winningGames = ListBuffer[GameState]()

  
  def fight( state : GameState ) : Unit = {
    
    // are there any winning games?
    if( compareCost( state ) ){
      
      val autoState = autoHit(state)
      
    
    // player turn
    // effects
    val nextState = calcEffects(autoState)
    if( nextState.boss.hp <= 0 ){
      log.info("win")
      winningGames += clone(nextState)
    }
    else {
    
      // get the valid spells that can be cast 
      val spells = canAfford( nextState, validSpells(nextState) )
      
      if( spells.isEmpty ){
        // if no spells / no mana - lose
        log.info("lose - no spells")
      }
      else {

        for( s <- spells ){
          
          val newState = clone(nextState)

          // cast spell 
          castSpell( newState, s )
          
          if( newState.boss.hp <= 0 ){
            log.info("win")
            winningGames += newState 
          }
          else {

            // boss turn
            val nextState = calcEffects(newState)
            if( nextState.boss.hp <= 0 ){
              log.info("win")
              winningGames += nextState 
            }
            else {
            
              // calc damage
              bossAttack( nextState )
              if( nextState.player.hp > 0 ){
                log.info("next round")
                fight(clone(nextState))
              }
              else {
                log.info("lose - dead")
                // player is dead
              }
            }
          }
        }
      }
    }
    }
    else {
      log.info("too expensive")
    }
  }
  
  def calcEffects( state : GameState ) : GameState = {
    
    val newState = clone(state)
    
    for( se <- newState.effects ){
      
      se.spell match {
        case Shield() => {
         newState.player.armor = se.spell.armor 
         se.remaining = se.remaining - 1
         if( se.remaining <= 0 ){
           newState.player.armor = 0
         }
        }
        case Poison() => {
          newState.boss.hp = newState.boss.hp - se.spell.damage 
          se.remaining = se.remaining - 1
        }
        case Recharge() => {
          newState.player.mana = newState.player.mana + se.spell.mana
          se.remaining = se.remaining - 1
        }
      }
    }
    
    newState.effects = newState.effects.filter( (se) => { se.remaining > 0 } )
    
    newState
  }
  
  def validSpells( state : GameState ) : List[Spell] = {
    val ineffect = state.effects.map( (se) => { se.spell })
    allSpells.filter( (s) => { !ineffect.contains(s) } )
  }

  def canAfford(state: GameState, validSpells: List[Spell]) = {
    validSpells.filter( (s) => { s.cost <= state.player.mana } )
  }

  def castSpell(newState: GameState, s: Spell) = {
    
    log.info("cast:"+ s.getClass.getName)
    
    newState.player.mana = newState.player.mana - s.cost 
    newState.manaSpent = newState.manaSpent + s.cost
    newState.spellsCast += s

    s match {
      case Missile() => {
        // hit boss
        newState.boss.hp = newState.boss.hp - s.damage 
      }
      case Drain() => {
        // hit boss
        newState.boss.hp = newState.boss.hp - s.damage  
        newState.player.hp = newState.player.hp + s.heal 
      }
      case Shield() => {
        // start shield
        newState.effects += new SpellEffect(Shield(), s.duration)
      }
      case Poison() => {
        // start poison
        newState.effects += new SpellEffect(Poison(),s.duration)
      }
      case Recharge() => {
        // start recharge
        newState.effects += new SpellEffect(Recharge(), s.duration)
      } 
    }
  
  }

  def bossAttack(newState: GameState) = {
    
    var damage = newState.boss.damage - newState.player.armor
    if( damage <= 0 ){
      damage = 1
    }
    
    newState.player.hp = newState.player.hp - damage
  }

  def compareCost(state: GameState) : Boolean = {
    if( winningGames.isEmpty ){
      true
    }
    else {
      !winningGames.exists( (g) => { g.manaSpent < state.manaSpent } )  
    }
  }

  def autoHit(state: GameState) = {
    val newState = clone(state)
    newState.player.hp = newState.player.hp - 1
    newState
  }

}