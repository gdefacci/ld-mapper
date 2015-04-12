package org.obl.ldmapper

import org.obl.raz._
import org.obl.raz.BasePath.toPathSegmentAdder

object Uris {

  val schemaOrg = HTTP("schema.org")
  val w3Org = HTTP("http://www.w3.org")
  val xmlnsCom = HTTP("xmlns.com")
  
//  def ns(f:String => Path):String => Path = {
//    f
//  }
  
  class Ns(private val f:String => Path) {
    def ns(sfx:String) = f(sfx)
  }

  object rdf extends Ns(w3Org / "1999" / "02" / "22-rdf-syntax-ns" &# _) {
    
    lazy val Property = ns("Property")

    lazy val rdfType = ns("type")
  }
  
  object xsd extends Ns(w3Org / "2001" / "XMLSchema" &# _) {
    lazy val string = ns("string")
    lazy val boolean = ns("boolean")
    lazy val decimal = ns("decimal")
    lazy val integer = ns("integer")
    lazy val long = ns("long")
    lazy val dateTime = ns("dateTime")
  }
  
  object rdfs extends Ns(w3Org / "2000" / "01" / "rdf-schema" &# _) {

    lazy val Class = ns("Class")
    lazy val label = ns("label")
    lazy val comment = ns("comment")
    lazy val domain = ns("domain")
    lazy val range = ns("range")
    lazy val Resource  = ns("Resource")
    lazy val subPropertyOf = ns("subPropertyOf")
    lazy val subClassOf = ns("subClassOf")
    lazy val seeAlso = ns("seeAlso")
    lazy val isDefinedBy = ns("isDefinedBy")
  }
  
  object owl extends Ns(w3Org / "2002" / "07" / "owl" &# _){

    lazy val Ontology = ns("Ontology")
  }

  object hydra extends Ns(w3Org / "ns" / "hydra" / "core" &# _) {

    lazy val ApiDocumentation = ns("ApiDocumentation")
    lazy val Class = ns("Class")
    lazy val Collection = ns("Collection")
    lazy val entrypoint = ns("entrypoint")
    lazy val expects = ns("expects")
    lazy val IriTemplate = ns("IriTemplate")
    lazy val IriTemplateMapping = ns("IriTemplateMapping")
    lazy val Link = ns("Link")
    lazy val mapping = ns("mapping")
    lazy val member = ns("member")
    lazy val method = ns("method")
    lazy val property = ns("property")
    lazy val Operation = ns("Operation")
    lazy val readonly = ns("readonly")
    lazy val Resource  = ns("Resource")
    lazy val required = ns("required")
    lazy val returns = ns("returns")
    lazy val StatusCodeDescription = ns("StatusCodeDescription")
    lazy val SupportedProperty = ns("SupportedProperty")
    lazy val statusCode = ns("statusCode")
    lazy val statusCodes = ns("statusCodes")
    lazy val supportedClass = ns("supportedClass")
    lazy val supportedOperation = ns("supportedOperation")
    lazy val supportedProperty = ns("supportedProperty")
    lazy val template = ns("template")
    lazy val TemplatedLink = ns("TemplatedLink")
    lazy val variable = ns("variable")
    lazy val writeonly = ns("writeonly")
  }
  
  object foaf extends Ns(xmlnsCom / "foaf" / "0.1" &# _) {
    lazy val name = ns("name")
  }

}