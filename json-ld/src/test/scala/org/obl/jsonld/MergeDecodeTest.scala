package org.obl.jsonld

import org.obl.raz._
import builder._
import junit.framework.TestCase
import org.junit.Test

class MergeDecodeTest extends TestCase with LdConversions {

  def jsonLdPrefix: String = "@"
  
  case class InfoA(a:String, b:Int)
  case class InfoB(a:String, c:String)
  
  object Paths {
    val host = HTTP("myapp.com")
    
    val a = host / "a"
    val b = host / "b"
    val c = host / "c"
    
  }
  
  val decA = LdDecode(
      LdFieldDecode[String](Paths.a), 
      LdFieldDecode[Int](Paths.b)).map(InfoA.tupled)
  
  val decB = LdDecode(
      LdFieldDecode[String](Paths.a), 
      LdFieldDecode[String](Paths.c)).map(InfoB.tupled)
  
  @Test
  def test1 = {
    val md = LdMergeDecode.mergeDecode(decA, decB)    
    
    val rawLd = """
    {
    "http://myapp.com/a": "str a",
    "http://myapp.com/b": 12,
    "http://myapp.com/c": "str c"
    }
      """

    rawLd.parseJsonLd(md) match {
      case scalaz.\/-((InfoA("str a", 12), InfoB("str a", "str c"))) => ()
      case _ => ???
    } 
    
  }
  
  
}