package org.obl.ldmapper
package hydra

import org.obl.raz._
import java.time.LocalDateTime

trait LdInputType[I] {
  def id:NodeId
}

trait LdOutputType[O] {
  def id:NodeId
}

trait LdType[I,O] { 
  def id:NodeId
  
  lazy val input:LdInputType[I]  = new LdInputType[I] {
    lazy val id = LdType.this.id
  }
  def output:LdOutputType[O]  =  new LdOutputType[O] {
    lazy val id = LdType.this.id
  }
}

object LdType {
  
  def apply[T](nd:NodeId) = new LdType[T,T] {
    def id = nd
  }
  
  val xsdInt = LdType[Int](Uris.xsd.integer)
  val xsdDecimal = LdType[Double](Uris.xsd.decimal)
  val xsdString = LdType[String](Uris.xsd.string)
  val xsdBoolean = LdType[Boolean](Uris.xsd.boolean)
  val xsdDateTime = LdType[LocalDateTime](Uris.xsd.dateTime)
  
  val hydraResource = LdType[Path](Uris.hydra.Resource)
  val hydraLink = LdType[Path](Uris.hydra.Link)
  val rdfsResource = LdType[NodeId](Uris.rdfs.Resource)
  
  val hydraCollection  = LdType[HydraCollection](Uris.hydra.Collection)

  def linkTo(l:NodeId) = LdType[Path](l)
  def linkTo(l:LdType[_,_]) = LdType[Path](l.id)
}

