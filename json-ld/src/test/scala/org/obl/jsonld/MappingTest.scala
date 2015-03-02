package org.obl.ldmapper

import org.obl.raz._
import builder._
import junit.framework.TestCase
import org.junit.Test

class MappingTest extends TestCase with LdConversions {

  def jsonLdPrefix: String = "@"

  class Ns(private val f: String => Path) {
    def ns(sfx: String) = f(sfx)
  }

  object foaf extends Ns(HTTP("xmlns.com") / "foaf" / "0.1" / (_: String)) {
    val name = ns("name")
    val homepage = ns("homepage")
    val openid = ns("openid")
    val img = ns("img")
  }

  case class OutPerson(id: Path, name: String, homepage: Path, img: Path, openid: Path)
  case class InPerson(name: String, homepage: Path, img: Path, openid: Path)

   lazy val person = {
    implicit val pathDecode = LdDecode.path
    implicit val pathEncode = LdEncode.path

    LdBuilder.
      add(LdFieldEncode.id).
      add[String](foaf.name).
      add[Path](foaf.homepage).
      add[Path](foaf.img).
      add[Path](foaf.openid)
  }

  @Test
  def testMapping = {

 
    implicit lazy val personEncode = person.toLdEncode.contramap[OutPerson](OutPerson.unapply(_).get)
    implicit lazy val personDecode = person.toLdDecode.map(InPerson.tupled)

    val pers1 = OutPerson(Raz / "id", "my name", Raz / "myHomepage", Raz / "img", Raz / "openid")

    assert(pers1.jsonLdRender() ==
      """{"@id":"/id","http://xmlns.com/foaf/0.1/name":["my name"],"http://xmlns.com/foaf/0.1/homepage":["/myHomepage"],"http://xmlns.com/foaf/0.1/img":["/img"],"http://xmlns.com/foaf/0.1/openid":["/openid"]}""")

    val str = """
{
  "http://xmlns.com/foaf/0.1/name" : ["my name"],
  "http://xmlns.com/foaf/0.1/homepage" : ["/myHomepage"],
  "http://xmlns.com/foaf/0.1/img" : ["/img"],
  "http://xmlns.com/foaf/0.1/openid" : ["/openid"],
  "http://xmlns.com/foaf/0.1/non" : ["/boyd"]
}
"""

    str.parseJsonLd[InPerson] match {
      case scalaz.\/-(v) => assert(v == InPerson("my name", Raz / "myHomepage", Raz / "img", Raz / "openid"))
      case _ => ???
    }

  }

}