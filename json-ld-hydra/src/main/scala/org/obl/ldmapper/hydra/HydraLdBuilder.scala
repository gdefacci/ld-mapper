package org.obl.ldmapper.hydra

import org.obl.ldmapper._
import org.obl.ldmapper.builder._

import org.obl.raz
import shapeless._
import scalaz.\/

object HydraClassBuilder extends HydraClassBuilder[HNil.type, HNil.type](HNil, HNil, Nil)

class HydraClassBuilder[HI <: HList, HO <: HList](input: HI, output: HO, supportedProperties:Seq[SupportedProperty]) {

  private def mkRdfProperty[T](range:NodeId, properties:Seq[JsonLdFieldValue[_]], required:Boolean, access: Access.AccessValue):SupportedProperty = {
    implicit val implEnc = LdEncode.nodeId
    
    SupportedProperty(
      RdfElement(properties ++ Seq(JsonLdFieldValue(LdField(Uris.rdfs.range), Seq(LdPrinter.toJsonLd(range)) ))),    
      required,
      access
    )
  }
  
  def property[O](p: LdFieldEncode[O], range:LdOutputType[O], properties:JsonLdFieldValue[_]*):HydraClassBuilder[HI, LdFieldEncode[O] :: HO] = 
    new HydraClassBuilder[HI, LdFieldEncode[O] :: HO](input, p :: output, supportedProperties ++ Seq(mkRdfProperty(range.id, properties, true, Access.writeOnly)))
    
  def property[I](p: LdFieldDecode[I], range:LdInputType[I], properties:JsonLdFieldValue[_]*):HydraClassBuilder[LdFieldDecode[I] :: HI, HO] = 
    new HydraClassBuilder[LdFieldDecode[I] :: HI, HO](p :: input, output, supportedProperties ++ Seq(mkRdfProperty(range.id, properties, true, Access.readOnly)))
  
  private def property[I, O](pd: LdFieldDecode[I], pe: LdFieldEncode[O], range:LdType[I,O], properties:Seq[JsonLdFieldValue[_]]) = 
    new HydraClassBuilder[LdFieldDecode[I] :: HI, LdFieldEncode[O] :: HO](pd :: input, pe :: output, supportedProperties ++ Seq(mkRdfProperty(range.id, properties, true, Access.readWrite)))

  def property[I](path: raz.Path, range:LdInputType[I], properties:JsonLdFieldValue[_]*)(implicit dec: LdDecode[I]): HydraClassBuilder[LdFieldDecode[I] :: HI, HO] =
    property[I](LdFieldDecode(path), range, properties:_*)

  def property[O](path: raz.Path, range:LdOutputType[O], properties:JsonLdFieldValue[_]*)(implicit dec: LdEncode[O]):HydraClassBuilder[HI, LdFieldEncode[O] :: HO] =
    property[O](LdFieldEncode[O](path), range, properties:_*)
    
  def property[I,O](path: raz.Path, range:LdType[I,O], properties:JsonLdFieldValue[_]*)(implicit dec: LdDecode[I], enc: LdEncode[O]): HydraClassBuilder[LdFieldDecode[I] :: HI, LdFieldEncode[O] :: HO] =
    property(LdFieldDecode(path), LdFieldEncode(path), range, properties)

  def property[T](f:JsonLdField[T], range:LdType[T,T], properties:JsonLdFieldValue[_]*)(implicit pd: LdDecode[T], pe: LdEncode[T]): HydraClassBuilder[LdFieldDecode[T] :: HI, LdFieldEncode[T] :: HO] = {
    property[T,T](LdFieldDecode.jsonldField(f), LdFieldEncode.jsonLdField(f), range, properties)
  }
  
  def link(path:raz.Path, range:NodeId, properties:JsonLdFieldValue[_]*):HydraClassBuilder[HI, LdFieldEncode[raz.Path] :: HO] =
    link(path, LdType.linkTo(range), properties:_*)
    
  def link(path:raz.Path, range:LdType[_,raz.Path], properties:JsonLdFieldValue[_]*) =
     property(LdFieldEncode.field(path, LdEncode.link), range.output, (RdfElement.ldType(Uris.hydra.Link) +: properties):_* )
  
  def create[I,O](id:NodeId)(implicit tdj: HRead[HI, I], tej: HWrite[HO, O]) = {
    new HydraLdType[I,O](id, tdj(input), tej(output), supportedProperties)
  }
  
}

class HydraLdType[I,O](val id:NodeId, decoder:LdDecode[I], encoder:LdEncode[O], supportedProperties:Seq[SupportedProperty]) extends LdDecode[I] with LdEncode[O] with LdType[I,O] {
  
  def tryEncode(t: O): Throwable \/ JsonLdModel = encoder.tryEncode(t)
  
  type LdEncodeSelf[T1] = HydraLdType[I,T1]
  protected def encoderFactory[T1](f: T1 => Throwable \/ JsonLdModel):HydraLdType[I,T1] = {
    new HydraLdType[I,T1](id, decoder, LdEncode[T1](f), supportedProperties)
  }
  
  def decode(j: JsonLdModel): Throwable \/ I = decoder.decode(j) 
    
  type LdDecodeSelf[T1] = HydraLdType[T1,O]
  def  decoderFactory[T1](f: JsonLdModel => Throwable \/ T1):HydraLdType[T1,O] =
    new HydraLdType(id, LdDecode(f), encoder, supportedProperties)

  private def addDomain(domain:NodeId):SupportedProperty => SupportedProperty = { prop =>
    var p1 = prop.property.copy( properties = Seq(RdfElement.domain(domain)) ++ prop.property.properties)
    prop.copy(property = p1)
  }

  def supportedClass[I,O](supportedOperation: Seq[Operation[_, _]], properties:JsonLdFieldValue[_]*):SupportedClass[I,O] = 
    SupportedClass[I,O](id, supportedProperties.map(addDomain(id)), supportedOperation, properties)

}