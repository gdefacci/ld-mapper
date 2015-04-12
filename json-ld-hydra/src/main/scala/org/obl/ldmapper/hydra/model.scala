package org.obl.ldmapper
package hydra

import org.obl.raz._

import Uris._

case class StatusCode(code: HttpStatuses.Status, description: String)

object HttpMethod extends Enumeration {
  val GET, POST, PUT, DELETE = Value
}

object Access extends Enumeration {
  case class AccessValue private[Access] (readonly: Boolean, writeonly: Boolean) extends Val
  
  def apply(readonly: Boolean, writeonly: Boolean) = {
   (values.collectFirst {
     case v @ AccessValue(r,w) if r == readonly && w == writeonly => v 
   }).get
  }

  val readWrite = AccessValue(false, false)
  val readOnly = AccessValue(true, false)
  val writeOnly = AccessValue(false, true)
}

case class SupportedProperty(property: RdfElement, required: Boolean, access: Access.AccessValue)
case class Operation[I,O](id:NodeId, method: HttpMethod.Value, expects: Option[LdInputType[I]], returns: Option[LdOutputType[O]], properties:Seq[JsonLdFieldValue[_]])

object Operation {
  def apply[I,O](id:NodeId, method: HttpMethod.Value, expects: LdInputType[I], returns: LdOutputType[O], properties:JsonLdFieldValue[_]*) =
    new Operation(id, method, Some(expects), Some(returns), properties)
  
  def apply[I](id:NodeId, method: HttpMethod.Value, expects: LdInputType[I], properties:JsonLdFieldValue[_]*) =
    new Operation[I,Void](id, method, Some(expects), None, properties)
  
  def apply[O](id:NodeId, method: HttpMethod.Value, returns: LdOutputType[O], properties:JsonLdFieldValue[_]*) =
    new Operation[Void, O](id, method, None, Some(returns), properties)
    
  def apply(id:NodeId, method: HttpMethod.Value, properties:JsonLdFieldValue[_]*) =
    new Operation[Void, Void](id, method, None, None, properties)  
}

case class SupportedClass[+I,+O](id: NodeId, supportedProperty: Seq[SupportedProperty], supportedOperation: Seq[Operation[_, _]], properties:Seq[JsonLdFieldValue[_]])
case class ApiDocumentation(id: Path, entryPoint: Path, supportedClass: Seq[SupportedClass[_,_]], properties:Seq[JsonLdFieldValue[_]])

case class IriTemplate(template: String, mapping: Seq[IriTemplateMapping])
case class IriTemplateMapping(variable: String, property: Path, required: Boolean)

case class HydraCollection(id:Path, member: Seq[Path])

case class EntryPoint(entryPoint:Path)