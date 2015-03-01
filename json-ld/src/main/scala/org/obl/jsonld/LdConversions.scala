package org.obl.jsonld

import scalaz.{ -\/, \/, \/- }

trait LdConversions {

  def jsonLdPrefix: String
//  def ldPrintPretty: Boolean
  
  implicit class ToLd[T](v: T)(implicit ej: LdEncode[T]) {
    def jsonLdRender(pretty:Boolean = false): String = ej.encode(v).map(mdl => LdPrinter.print(mdl, jsonLdPrefix, pretty)) match {
      case -\/(err) => throw new Exception(s"Error rendering to jsonld \nobject:$v\nerror:$err")
      case \/-(v) => v
    }
  }
  
  implicit class FromLd(text: String) {
    def parseJsonLd[T](implicit d:LdDecode[T]) = readJsonLd[T](text)
  }
  
  def readJsonLd[T](v: String)(implicit ej: LdDecode[T]): String \/ T = 
    LdReader.fromString(jsonLdPrefix).read(v, LdReadStrategy.Expanded).flatMap(jsonModel => ej.decode(jsonModel))
}