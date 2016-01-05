
import java.security.MessageDigest

object Day4 {

  val input = "ckczppom"

  def main( args : Array[String] ) : Unit = {
    Console.println( "day4" )

    var found = false

    var suffix = 0

    while( !found ){

      var msg = input + suffix.toString
      val hash = MessageDigest.getInstance("MD5").digest( msg.getBytes)
      val md5hash1 = hash.map("%02x".format(_)).mkString
      Console.println(md5hash1)

      if( md5hash1.startsWith("000000") ){
        found = true
      }
      else {
        suffix = suffix + 1
      }
    }

    Console.println(suffix)

  }
}