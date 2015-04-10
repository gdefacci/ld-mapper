package org.obl.ldmapper

import org.obl.raz._
import builder._
import junit.framework.TestCase
import org.junit.Test

class MergeEncodeTest extends TestCase {

  def jsonLdPrefix: String = "@"
  
  case class InfoA(a:String, b:Int)
  case class InfoB(a:String, c:String)
  
  object Paths {
    val host = HTTP("myapp.com")
    
    val a = host / "a"
    val b = host / "b"
    val c = host / "c"
    
  }
  
  val infAEnc = LdEncode(
    LdFieldEncode[String](Paths.a),    
    LdFieldEncode[Int](Paths.b)    
  ).contramap[InfoA](InfoA.unapply(_).get)
  
  val infBEnc = LdEncode(
    LdFieldEncode[String](Paths.a),    
    LdFieldEncode[String](Paths.c)    
  ).contramap[InfoB](InfoB.unapply(_).get)
  
  @Test
  def test1 = {
    
    val i1 = InfoA("str a1", 12)
    val i2 = InfoB("str a2", "c prop")
    
    implicit val writeOpts = LdPrintOptions("@", false)
    implicit val me = LdMergeEncode.merge(infAEnc, infBEnc)
    
    val exp = """{"http://myapp.com/c":["c prop"],"http://myapp.com/b":[12.0],"http://myapp.com/a":["str a1","str a2"]}"""
    
    assert( LdPrinter.render(i1 -> i2) == exp )
    
  }
  

}