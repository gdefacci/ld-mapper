package org.obl.ldmapper
package hydra

import org.obl.raz.Path

object HydraLdEncode  {
  
  import Uris.{ rdfs, hydra => hydraNs }

  implicit lazy val httpStatusCodeLdEncode = LdEncode.int.contramap { st: HttpStatuses.Status => st.code }

  implicit lazy val statusCodeLdEncode: LdEncode[StatusCode] = LdEncode(
    LdFieldEncode[HttpStatuses.Status](hydraNs.statusCode),
    LdFieldEncode[String](rdfs.comment)).contramap((p: StatusCode) => StatusCode.unapply(p).get)

  implicit lazy val httpMethodLdEncode = LdEncode.enum[HttpMethod.type]  
  
  implicit lazy val httpMethod =
    LdFieldEncode[HttpMethod.Value](hydraNs.method) 
  
  implicit lazy val rdfElementEncode = LdEncode.jsonLdFields.contramap[RdfElement](e => e.properties)
  
  private val statusCodes = LdFieldEncode.set[StatusCode](hydraNs.statusCodes)
  
//  def operationLdEncode[I,O]:LdEncode[Operation[I,O]] = LdEncode(
//    LdFieldEncode.nodeId,
//    httpMethod,
//    LdFieldEncode[NodeId](hydraNs.expects)(LdEncode.nodeIdLink).contramap((t:LdType[I]) => t.id).opt,
//    LdFieldEncode[NodeId](hydraNs.returns)(LdEncode.nodeIdLink).contramap((t:LdType[O]) => t.id).opt,
//    LdFieldEncode.jsonLdFields).contramap((p: Operation[I,O]) => Operation.unapply(p).get)
  
  def operationLdEncode:LdEncode[Operation[_,_]] = LdEncode(
    LdFieldEncode.nodeId,
    httpMethod,
    LdFieldEncode[NodeId](hydraNs.expects)(LdEncode.nodeIdLink).contramap((t:LdInputType[_]) => t.id).opt,
    LdFieldEncode[NodeId](hydraNs.returns)(LdEncode.nodeIdLink).contramap((t:LdOutputType[_]) => t.id).opt,
    LdFieldEncode.jsonLdFields).contramap((p: Operation[_,_]) => Operation.unapply(p).get)
    
    
  implicit val supportedPropertyLdEncode = LdEncode(
    LdFieldEncode[RdfElement](hydraNs.property),
    LdFieldEncode[Boolean](hydraNs.required),
    LdFieldEncode[Boolean](hydraNs.readonly),
    LdFieldEncode[Boolean](hydraNs.writeonly)
  ).contramap((p: SupportedProperty) => (p.property, p.required, p.access.readonly, p.access.writeonly))  

  implicit def supportedClassLdEncode = LdEncode.ldtype(Set[NodeId](hydraNs.Class), LdEncode(
    LdFieldEncode.nodeId,
    LdFieldEncode.set[SupportedProperty](hydraNs.supportedProperty),
    LdFieldEncode.set[Operation[_,_]](hydraNs.supportedOperation)(operationLdEncode),
    LdFieldEncode.jsonLdFields
  ).contramap((p: SupportedClass[_,_]) => SupportedClass.unapply(p).get))

  
//
//  private lazy val ontologyElemTupleLdEncode = LdEncode(
//    LdFieldEncode.id,
//    LdFieldEncode.ldtype,
//    label,
//    description.opt,
//    LdFieldEncode[NodeId](rdfs.domain)(LdEncode.nodeIdLink).opt,
//    LdFieldEncode[NodeId](rdfs.range)(LdEncode.nodeIdLink).opt,
//    LdFieldEncode[Path](rdfs.subPropertyOf)(LdEncode.link).opt,
//    LdFieldEncode[NodeId](rdfs.subClassOf)(LdEncode.nodeIdLink).opt,
//    LdFieldEncode[Path](rdfs.seeAlso)(LdEncode.link).opt)
// 
//  implicit lazy val ontologyElemLdEncode:LdEncode[OntologyElem] = {
//     ontologyElemTupleLdEncode.contramap[OntologyElem]( (p:OntologyElem) => (p.id,p.typ,p.label,p.description,p.domain,p.range,p.subPropertyOf,p.subClassOf,p.seeAlso))
//  }   
  
//  private implicit def propertyLdEncode:LdEncode[RDFProperty] = {
//    val suppOps = LdFieldEncode.set[Operation](hydraNs.supportedOperation)
//    val ec = ontologyElemTupleLdEncode.contramap[RDFProperty]( (p:RDFProperty) => (p.id,p.typ,p.label,p.description,p.domain,p.range,p.subPropertyOf,p.subClassOf,p.seeAlso))
//    LdEncode.append[RDFProperty]( ec, (p:RDFProperty) => suppOps.tryEncode(p.supportedOperation))
//  }   
//
//  private implicit val iriTemplateMappingLdEncode = LdEncode(
//    LdFieldEncode[String](hydraNs.variable),
//    LdFieldEncode[Path](hydraNs.property)(LdEncode.link),
//    LdFieldEncode[Boolean](hydraNs.required)
//  ).contramap( (p:IriTemplateMapping) => IriTemplateMapping.unapply(p).get )
//
//  implicit val iriTemplateLdEncode = LdEncode.ldtype[IriTemplate]((it:IriTemplate) => Set[NodeId](Uris.hydra.TemplatedLink), LdEncode(
//    LdFieldEncode[String](Uris.hydra.template),
//    LdFieldEncode.set[IriTemplateMapping](hydraNs.mapping)
//  ).contramap( (p:IriTemplate) => IriTemplate.unapply(p).get)  )
//  
//  private implicit val supportedPropertyLdEncode = LdEncode(
//    LdFieldEncode[RDFProperty](hydraNs.property),
//    LdFieldEncode[Boolean](hydraNs.required),
//    LdFieldEncode[Boolean](hydraNs.readonly),
//    LdFieldEncode[Boolean](hydraNs.writeonly)
//  ).contramap((p: SupportedProperty) => (p.property, p.required, p.access.readonly, p.access.writeonly))  
//  
//  private implicit val supportedClassLdEncode = LdEncode.ldtype(Set[NodeId](hydraNs.Class), LdEncode(
//    LdFieldEncode.nodeId,
//    label,
//    description.opt,
//    LdFieldEncode.set[SupportedProperty](hydraNs.supportedProperty),
//    LdFieldEncode.set[Operation](hydraNs.supportedOperation)
//  ).contramap((p: SupportedClass) => SupportedClass.unapply(p).get))
//    
//  implicit val apiDocumentationLdEncode = LdEncode.ldtype(Set[NodeId](hydraNs.ApiDocumentation), LdEncode(
//    LdFieldEncode.id,
//    label,
//    description.opt,
//    LdFieldEncode[Path](hydraNs.entrypoint)(LdEncode.link),
//    LdFieldEncode.set[SupportedClass](hydraNs.supportedClass),
//    statusCodes) 
//  ).contramap((p: ApiDocumentation) => ApiDocumentation.unapply(p).get)
//
//  
//  implicit val hydraCollectionLdEncode:LdEncode[HydraCollection] = LdEncode.ldtype(Set[NodeId](hydraNs.Collection), LdEncode(
//	LdFieldEncode.id,
//	LdFieldEncode.set[Path](hydraNs.member)(LdEncode.link))
//  ).contramap((p:HydraCollection) => HydraCollection.unapply(p).get)
//
//  
//  implicit def ontologyLdEncode[T](implicit encld:LdEncode[T]):LdEncode[Ontology[T]] = { 
//    implicit val ontologyElemsList = encld.set
//    LdEncode.ldtype( Set[NodeId](Uris.owl.Ontology), LdEncode(
//    LdFieldEncode.id,
//    LdFieldEncode.reverse[Seq[T]](Uris.rdfs.isDefinedBy)).contramap((p:Ontology[T]) => Ontology.unapply(p).get) )
//  }
    
}