package org.obl.ldmapper.hydra

import org.obl.raz._
import org.obl.ldmapper._

case class RdfElement(properties: Seq[JsonLdFieldValue[_]]) {
  lazy val toJsonLd: JsonLdModel = {
    LdObject(properties)
  }
}

object RdfElement  {
  def property[T](field: JsonLdField[T], value: T) = JsonLdFieldValue(field, value)
  def property[T](path: Path, value: Seq[JsonLdModel]) = JsonLdFieldValue(LdField(path), value)

  import LdPrinter.toJsonLd
  
  implicit val pathEncode = LdEncode.path
  implicit val nodeIdEncode = LdEncode.nodeId
  implicit val statusCodeLdEncode = HydraLdEncode.statusCodeLdEncode

  def ldType(typs:NodeId*) = property(TypeJsonLdField, typs.toSet) 
  
  def comment(p: String) = property(Uris.rdfs.comment, Seq(toJsonLd(p)))
  def label(p: String) = property(Uris.rdfs.label, Seq(toJsonLd(p)))
  def range(p: NodeId*) = property(Uris.rdfs.range, p.map(toJsonLd(_)))
  def domain(p: NodeId) = property(Uris.rdfs.domain, Seq(toJsonLd(p)))
  def subClassOf(p: NodeId) = property(Uris.rdfs.subClassOf, Seq(toJsonLd(p)))
  def subPropertyOf(p: NodeId) = property(Uris.rdfs.subPropertyOf, Seq(toJsonLd(p)))
  def seeAlso(p: NodeId) = property(Uris.rdfs.seeAlso, Seq(toJsonLd(p)))
  
  def entryPoint(p: Path) = property(Uris.hydra.entrypoint, Seq(toJsonLd(p)))

  private def encodeOp[I,O](op:Operation[I,O]) = HydraLdEncode.operationLdEncode.encode(op)
  
  def supportedOperations(hd:Operation[_,_], ops: Operation[_,_]*) = property(Uris.rdfs.seeAlso, (hd +: ops).map( (op:Operation[_,_]) => encodeOp(op) ))
  def statusCodes(hd:StatusCode, codes: StatusCode*) = property(Uris.hydra.statusCodes, (hd +: codes).map(toJsonLd(_)))

}
  
