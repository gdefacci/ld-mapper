package org.obl.ldmapper

import scalaz.{ -\/, \/, \/- }

import scala.language.implicitConversions

class ToLd[T](v: T)(implicit ej: LdEncode[T]) {
  def toJsonLd: JsonLdModel = ej.encode(v) match {
    case -\/(err) => throw new Exception(s"Error rendering to jsonld \nobject:$v\nerror:$err")
    case \/-(v) => v
  }
}

object ToLd {
  implicit def apply[T](v: T)(implicit ej: LdEncode[T]) = {
    new ToLd(v)
  }
  
  def toJsonLd[T](v:T)(implicit ej: LdEncode[T]):JsonLdModel = ToLd(v).toJsonLd
}

trait LdConversions {

  def jsonLdPrefix: String
//  def ldPrintPretty: Boolean
  
  implicit class ToLd[T](v: T)(implicit ej: LdEncode[T]) extends org.obl.ldmapper.ToLd[T](v) {
    
    def jsonLdRender(pretty:Boolean = false): String = ej.encode(v).map(mdl => LdPrinter.print(mdl, jsonLdPrefix, pretty)) match {
      case -\/(err) => throw new Exception(s"Error rendering to jsonld \nobject:$v\nerror:$err")
      case \/-(v) => v
    }
    
  }
  
  implicit class FromLd(text: String) {
    def parseJsonLd[T](implicit d:LdDecode[T]) = readJsonLd[T](text)
  }
  
  def readJsonLd[T](v: String)(implicit ej: LdDecode[T]): Throwable \/ T = 
    LdReader.fromString(jsonLdPrefix).read(v, LdReadStrategy.Expanded).flatMap(jsonModel => ej.decode(jsonModel))
}