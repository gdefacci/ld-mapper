package org.obl.ldmapper

import org.obl.raz._
import org.junit.Test
import junit.framework.TestCase
import org.obl.ldmapper.NodeId.apply
import org.obl.raz.BasePath.toPathSegmentAdder

import scalaz.{-\/, \/, \/-}

class EncodeTest extends TestCase {

  case class Cl0(nicks: Seq[String])
  case class Minni(name: String, pluto: Pluto, nicks: Seq[String])
  case class Pluto(name: String, age: Int)

  val r = HTTP("my-site.com") / "myapp"

  def checkSym[T](m:T, enc:LdEncode[T], dec:LdDecode[T]) {
	    var ld = enc.tryEncode(m).toOption.get
      val r = dec.decode(ld) 
      r match {
        case -\/(err) => throw new RuntimeException(err)
        case \/-(v) => assert(m == v)
      }
	  }
  
  object Codecs1 {
	  
	  implicit val plutoEnc = LdEncode(
	      LdFieldEncode[String](r / "name"), 
	      LdFieldEncode[Int](r / "age")).contramap { p: Pluto => Pluto.unapply(p).get }
	  
	  implicit val minniEnc = LdEncode(
	    LdFieldEncode[String](r / "name"),
	    LdFieldEncode[Pluto](r / "pluto"),
	    LdFieldEncode.set[String](r / "nick")).contramap { p: Minni => Minni.unapply(p).get }
	
	  implicit val plutoDec = LdDecode(
	    LdFieldDecode[String](r / "name"),
	    LdFieldDecode[Int](r / "age")).map(Pluto.tupled)
	
	  implicit val minniDec = LdDecode(
	    LdFieldDecode[String](r / "name"),
	    LdFieldDecode[Pluto](r / "pluto"),
	    LdFieldDecode.set[String](r / "nick")).map(Minni.tupled)
	
  }
  
  object Codecs2 {
	  implicit val plutoEnc = LdEncode(
	      LdFieldEncode[String](r / "name"), 
	      LdFieldEncode[Int](r / "age")).contramap { p: Pluto => Pluto.unapply(p).get }
	  
	  implicit val minniEnc = LdEncode(
	    LdFieldEncode[String](r / "name"),
	    LdFieldEncode[Pluto](r / "pluto")(LdEncode.linkTo((p:Pluto) => r / "pluto" / p.name)),
	    LdFieldEncode.set[String](r / "nick")).contramap { p: Minni => Minni.unapply(p).get }
	
  }
  
  object Codecs3 {
	 implicit val plutoEnc = LdEncode(
	      LdFieldEncode[String](r / "name"), 
	      LdFieldEncode[Int](r / "age")).contramap { p: Pluto => Pluto.unapply(p).get }
	  
	  implicit val minniEnc = LdEncode(
	    LdFieldEncode.reverse[String](r / "name"),
	    LdFieldEncode.reverse[Pluto](r / "pluto"),
	    LdFieldEncode.set[String](r / "nick")).contramap { p: Minni => Minni.unapply(p).get }
	
	  implicit val plutoDec = LdDecode(
	    LdFieldDecode[String](r / "name"),
	    LdFieldDecode[Int](r / "age")).map(Pluto.tupled)
	
	  implicit val minniDec = LdDecode(
	    LdFieldDecode.reverse[String](r / "name"),
	    LdFieldDecode.reverse[Pluto](r / "pluto"),
	    LdFieldDecode.set[String](r / "nick")).map(Minni.tupled)

  }
  
  object Codecs6 {
    implicit val plutoEnc = LdEncode(
        LdFieldEncode[String](r / "name"), 
        LdFieldEncode[Int](r / "age")).contramap { p: Pluto => Pluto.unapply(p).get }
    
    implicit val minniEnc = LdEncode(
      LdFieldEncode[String](r / "name"),
      LdFieldEncode[Pluto](r / "pluto"),
      LdFieldEncode.list[String](r / "nick")).contramap { p: Minni => Minni.unapply(p).get }
  
    implicit val plutoDec = LdDecode(
      LdFieldDecode[String](r / "name"),
      LdFieldDecode[Int](r / "age")).map(Pluto.tupled)
  
    implicit val minniDec = LdDecode(
      LdFieldDecode[String](r / "name"),
      LdFieldDecode[Pluto](r / "pluto"),
      LdFieldDecode.list[String](r / "nick")).map(Minni.tupled)
  
  }
    
  @Test
  def test0: Unit = {
    val cl0Enc = LdEncode(LdFieldEncode.set[String](r / "nick")).contramap { p: Cl0 => Cl0.unapply(p).get }
    val cl0Dec = LdDecode(LdFieldDecode.set[String](r / "nick")).map(Cl0.apply)
    
	  val cl0 = Cl0(Seq("mi", "mo", "mah"))
	  val cl0Ld:JsonLdModel = cl0Enc.tryEncode(cl0).toOption.get

	  checkSym(cl0, cl0Enc, cl0Dec)

    val inpModel =
      LdObject(Seq(
        JsonLdFieldValue(LdField(r / "nick"), Seq( LdString("mi"), LdString("mo"), LdString("mah") ))    
      ))

    assert(cl0Ld != inpModel)
    
    val cl0a = cl0Dec.decode(inpModel).toOption.get
    assert(cl0a == cl0)
    
  }
  
  @Test
  def test0a: Unit = {
    val cl0Enc = LdEncode(LdFieldEncode.list[String](r / "nick")).contramap { p: Cl0 => Cl0.unapply(p).get }
    val cl0Dec = LdDecode(LdFieldDecode.list[String](r / "nick")).map(Cl0.apply)
    
    val cl0 = Cl0(Seq("mi", "mo", "mah"))
    val cl0Ld = cl0Enc.tryEncode(cl0).toOption.get
    
    checkSym(cl0, cl0Enc, cl0Dec)
  }
  
  implicit val ldPrintOptions = LdPrintOptions("@", true)

  @Test
  def test1: Unit = {
    import Codecs1 ._
    
    val minni = Minni("minni", Pluto("pluto", 12), Seq("mo", "ea"))

    val minniLd = minniEnc.tryEncode(minni).toOption.get

    println( LdPrinter.print(minniLd) )
    
    checkSym(minni, minniEnc, minniDec)
  }
  
  @Test
  def test2: Unit = {
    import Codecs2 ._
    
    val minni = Minni("minni", Pluto("pluto", 12), Seq("mo", "ea"))

    val minniLd = minniEnc.tryEncode(minni).toOption.get
    
    implicit val ldPrintOptions = LdPrintOptions("@", false)

    assert(
      LdPrinter.print(minniLd) ==
      """{"http://my-site.com/myapp/nick":["mo","ea"],"http://my-site.com/myapp/name":["minni"],"http://my-site.com/myapp/pluto":[{"@id":"http://my-site.com/myapp/pluto/pluto"}]}"""     
    )
  }
  
  @Test
  def testReverse = {
    import Codecs3 ._
    
    val minni = Minni("minni", Pluto("pluto", 12), Seq("mo", "ea"))

    val minniLd = minniEnc.tryEncode(minni).toOption.get

    println( LdPrinter.print(minniLd) )
    
    checkSym(minni, minniEnc, minniDec)
  }

  case class ClH[T](value:Option[T])
  
  object Codecs4 {
    
    val enc = LdEncode( LdFieldEncode[Int](r / "clh").opt).contramap((iclh:ClH[Int]) => iclh.value)
    val dec = LdDecode( LdFieldDecode[Int](r / "clh").opt()).map(ClH.apply)
    
  }
  
  object Codecs5 {
    
    implicit val plutoEnc = LdEncode(
	      LdFieldEncode[String](r / "name"), 
	      LdFieldEncode[Int](r / "age")).contramap { p: Pluto => Pluto.unapply(p).get }

    implicit val plutoDec = LdDecode(
	    LdFieldDecode[String](r / "name"),
	    LdFieldDecode[Int](r / "age")).map(Pluto.tupled)

    
    val enc = LdEncode( LdFieldEncode.reverse[Pluto](r / "clh").opt).contramap((iclh:ClH[Pluto]) => iclh.value)
    val dec = LdDecode( LdFieldDecode.reverse[Pluto](r / "clh").opt()).map(ClH.apply)
    
  }
  
  @Test
  def testOptionInt:Unit = {
    
    import Codecs4._
    
    val i1 = ClH(Some(156))
    val i2:ClH[Int] = ClH(None)
    
    checkSym(i1, enc, dec)
    checkSym(i2, enc, dec)
    
  }
  
  @Test
  def testRevOptionInt:Unit = {
    
    import Codecs5._
    
    val i1 = ClH(Some(Pluto("nice", 12)))
    val i2:ClH[Pluto] = ClH(None)
    
    checkSym(i1, enc, dec)
    checkSym(i2, enc, dec)
    
  }
 
  object Codecs7 {
    case class C1(a:Int)   
    case class C2(a:String)   
    
    val c1Enc = LdEncode(
        LdFieldEncode[Int](r / "a")).contramap[C1] { C1.unapply(_).get }
    val c2Enc = LdEncode(
        LdFieldEncode[String](r / "a")).contramap[C2] { C2.unapply(_).get }
    
    val c1Dec = LdDecode(
      LdFieldDecode[Int](r / "a")).map(C1.apply)
    val c2Dec = LdDecode(
      LdFieldDecode[String](r / "a")).map(C2.apply)

    val c1OrC2Dec:LdDecode[C1 \/ C2] = LdDecode.either[C1,C2](c1Dec, c2Dec)
    val c1OrC2Enc = LdEncode.either(c1Enc, c2Enc)
    
  }
  
  @Test
  def testEither = {
    
    import Codecs7._
    
    checkSym(\/-(C2("bauu")), c1OrC2Enc, c1OrC2Dec)
    checkSym(-\/(C1(544)), c1OrC2Enc, c1OrC2Dec)
    
     
    
  }

}