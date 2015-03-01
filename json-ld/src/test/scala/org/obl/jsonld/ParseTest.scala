package org.obl.jsonld

import org.obl.raz._
import org.junit.Test
import org.obl.raz.BasePath.toAbsolutePathFactory
import org.obl.raz.BasePath.toFragmentAdder
import org.obl.raz.BasePath.toPathSegmentAdder
import junit.framework.TestCase

class ParseTest extends TestCase {

  @Test
  def testParse1 {
      val str = """
{
  "@context":
  {
    "foaf": "http://xmlns.com/foaf/0.1/"
  },
  "@id": "http://example.org/people#joebob",
  "@type": "foaf:Person",
  "foaf:name": "Joe Bob",
  "@index":"it",
  "@language":"it",
  "foaf:nick":[
  {
    "@list": [ "joe", "bob", "joe", "jaybee" ]
  }, {
    "@list": [ "pluto", "minni"]
  }, {
    "foaf:name": "Razy",
    "@index":"de",
    "@language":"it"
  }]
}
"""
    
    val xmlNsCom = HTTP("xmlns.com")
    val exampleOrg = HTTP("example.org")
    
    val foaf = (Raz / "foaf" / "0.1").at(xmlNsCom)
    
    val r = LdReader.fromString().read(str, LdReadStrategy.Expanded).toOption.get
    
    assert( r.id.get.render ==  ((Raz / "people") ## "joebob").at(exampleOrg).render )
    assert( r.language.get ==  Language("it") )
    assert( r.index.get ==  "it" )
    assert( r.ldtype ==  Set(PathNodeId(foaf / "Person")) )

    val robj = r.obj.get
    assert(robj.get(foaf / "name").get(0).leaf.get.value == "Joe Bob")
    
    val nicks = robj.get(foaf / "nick")
    val nicks1 = nicks.get(0).container.get.elements
    val nicks2 = nicks.get(1).container.get.elements
    val nick3 = nicks.get(2).obj.get
    assert(nicks1.length == 4)
    assert(nicks2.length == 2)
    
    assert( nicks1.map(_.leaf.get.value) == Seq("joe", "bob", "joe", "jaybee") )
    assert( nicks2.map(_.leaf.get.value) == Seq("pluto", "minni") )
    
    assert(nick3.get(foaf / "name").get(0).leaf.get.value == "Razy")
    assert(nick3.index.get == "de")
    assert(nick3.language.get == Language("it"))
    
    
    val sw = new java.io.StringWriter()
    new LdPrinter("@").print(r, sw, false)
    assert( 
        sw.getBuffer().toString() ==
        """{"http://xmlns.com/foaf/0.1/name":["Joe Bob"],"@language":"it","http://xmlns.com/foaf/0.1/nick":[{"@list":["joe","bob","joe","jaybee"]},{"@list":["pluto","minni"]},{"@index":"de","@language":"it","http://xmlns.com/foaf/0.1/name":["Razy"]}],"@type":["http://xmlns.com/foaf/0.1/Person"],"@index":"it","@id":"http://example.org/people#joebob"}"""      
    )
    
  }
}