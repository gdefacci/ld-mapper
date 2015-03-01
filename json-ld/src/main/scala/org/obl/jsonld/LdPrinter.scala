package org.obl.jsonld

import com.fasterxml.jackson.core._
import java.io.Writer
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter.Indenter
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter.NopIndenter
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter.Lf2SpacesIndenter
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter.FixedSpaceIndenter

object LdPrinter {
  def print(t:JsonLdModel, prefix:String = "@", pretty:Boolean = true):String = {
    Util.use(new java.io.StringWriter()) { sw =>
    	new LdPrinter(prefix).print(t, sw, pretty)
    	sw.getBuffer.toString
    }
  }
}

class LdPrinter(prefix:String) {

  def print(m:JsonLdModel, out:Writer, pretty:Boolean):Unit = {
    val indenter = if (pretty) Lf2SpacesIndenter.instance else NopIndenter.instance
    val arrIndenter = if (pretty) FixedSpaceIndenter.instance else NopIndenter.instance
    val jackf = new JsonFactory()
    val gen = jackf.createGenerator(out)
    val jacksonPrinter = new DefaultPrettyPrinter().withObjectIndenter(indenter).withArrayIndenter(arrIndenter)
    gen.setPrettyPrinter(if (pretty) jacksonPrinter.withSpacesInObjectEntries() else jacksonPrinter.withoutSpacesInObjectEntries());
    print(m, gen)
    gen.flush();
  }
  
  private def print(m:JsonLdModel, out:JsonGenerator):Unit = {
    m.fold(printPrimitive(_, out), o => printObject(o, out), o => printContainer(o, out))
  }
  
  private def printPrimitive(m:LdPrimitive, gen:JsonGenerator) {
    val flds = m.fields
    if (flds.length==0) throw new Exception("primitive without any field "+m)
    else if (flds.length==1) {
      m.value match {
        case str:String => gen.writeString(str)
        case d:Double => gen.writeNumber(d)
        case b:Boolean => gen.writeBoolean(b)
        case x => throw new Exception("unexpecyted primitve value " + x)
      }
    } else {
      printFields(m.fields, gen)
    }
  }
  
  private def printArrayField[T](nm:String, arr:Seq[T], gen:JsonGenerator, f:(T,JsonGenerator) => Unit) = {
    gen.writeArrayFieldStart(nm)
    arr.foreach( e => f(e,gen))
    gen.writeEndArray();
  }
  
  private def printObject(ldobj:LdObject, gen:JsonGenerator) {
    lazy val default = printFields(ldobj.fields, gen)
    val flds = ldobj.fields
    flds match {
      case hd +: Seq() => hd match {
        case JsonLdFieldValue(SetJsonLdField, els) => {
          gen.writeStartArray()
          els.foreach(  print(_, gen) )
          gen.writeEndArray()
        }
        case x => default
      }
      case _=> default
    }
  }
  
  def innerContainer(container:LdContainer):LdContainer = {
    if (container.containerKind == LdContainerKind.set && container.fields.length == 1) {
      container.elements match {
        case (ldc:LdContainer) :+ Seq() => innerContainer(ldc)
        case _ => container
      }
    } else 
      container
  }
  
  private def printContainer(container:LdContainer, gen:JsonGenerator) {
    if (container.containerKind == LdContainerKind.set && container.fields.length == 1) {
      gen.writeStartArray()
      innerContainer(container).elements.foreach( e => print(e,gen))
      gen.writeEndArray()
    } else {
      printFields(container.fields, gen)
    }
  }
  
  private def printFields(fields:Seq[JsonLdFieldValue[_]], gen:JsonGenerator) {
    gen.writeStartObject()
    fields.foreach {
      case JsonLdFieldValue(k @ IdJsonLdField, id) => gen.writeStringField(k.render(prefix), id.render)
      case JsonLdFieldValue(k @ TypeJsonLdField, typs) => {
        printArrayField[String](k.render(prefix), typs.toSeq.map(_.render), gen, (e,gen) => gen.writeString(e))
      }
      case JsonLdFieldValue(k @ LdField(path), Seq(LdContainer(LdContainerKind.set, els, None))) => {
        printArrayField[JsonLdModel](path.render, els, gen, (e,gen) => print(e, gen))
      }
      case JsonLdFieldValue(k @ LdField(path), els) => {
        printArrayField[JsonLdModel](path.render, els, gen, (e,gen) => print(e, gen))
      }
      case JsonLdFieldValue(k @ ReverseJsonLdField, rev) => {
        gen.writeFieldName(k.render(prefix))
        print(rev, gen)
      }
      case JsonLdFieldValue(k @ IndexJsonLdField, index) => gen.writeStringField(k.render(prefix), index)
      case JsonLdFieldValue(k @ LanguageJsonLdField, lang) => gen.writeStringField(k.render(prefix), lang.code)
      case JsonLdFieldValue(k :ElementsJsonLdField, els) => {
        printArrayField[JsonLdModel](k.render(prefix), els, gen, (e,gen) => print(e, gen))
      }
    }
    gen.writeEndObject()
  }
  
}