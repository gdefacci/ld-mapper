package org.obl.ldmapper

import org.obl.raz.Path
import collection.mutable.Buffer

sealed trait JsonLdModel {
  
  def id:Option[NodeId]
  def ldtype:Set[NodeId]

  def index:Option[String]
  def language:Option[Language]
  
  def fold[T](leaf:LdPrimitive => T, obj:LdObject => T, container:LdContainer => T):T = {
    this match {
      case p:LdPrimitive => leaf(p)
      case o:LdObject => obj(o)
      case c:LdContainer => container(c)
    }
  }
  
  def container:Option[LdContainer] = fold[Option[LdContainer]](v => None, o => None, v => Some(v))
  def leaf:Option[LdPrimitive] = fold[Option[LdPrimitive]](v => Some(v), o => None, v => None)
  def obj:Option[LdObject] = fold[Option[LdObject]](v => None, o => Some(o), v => None)
  
  def number:Option[LdNumber] = leaf.collect({
    case ldn:LdNumber => ldn
  })
  
  def boolean:Option[LdBoolean] = leaf.collect({
    case ldn:LdBoolean => ldn
  })
  
  def string:Option[LdString] = leaf.collect({
    case ldn:LdString => ldn
  })
  
  def fields:Seq[JsonLdFieldValue[_]] = {
    id.map(JsonLdFieldValue(IdJsonLdField,_)).toSeq ++ 
    (if (ldtype.isEmpty) Nil else Seq(JsonLdFieldValue(TypeJsonLdField, ldtype))) ++
    index.map(JsonLdFieldValue(IndexJsonLdField,_)).toSeq ++
    language.map(JsonLdFieldValue(LanguageJsonLdField,_)).toSeq
  }
  
  private lazy val fieldsMap:Map[JsonLdField[_], Any] = {
    var res = collection.mutable.Map.empty[JsonLdField[_], Any]
    fields.foreach( f => res.put(f.field, f.value))
    res.toMap
  }
  
  def getField[T1](k:JsonLdField[T1]):Option[T1] = {
    fieldsMap.get(k).map(_.asInstanceOf[T1])
  }
  
  override def toString:String =
    s"{ ${fields.map(_.toString()).mkString(", ")} }"
}

private object JsonLdKeywords {
  val id = "id"
  val ldtype = "type"
  val reverse = "reverse"
  val graph = "graph"
  val value = "value"
  val language = "language"
  val container = "container"
  val list = "list"
  val set = "set"
  val index = "index"
}

sealed trait JsonLdField[T]

case class LdField(path:Path) extends JsonLdField[Seq[JsonLdModel]]

sealed trait KeywordLdField[T] extends JsonLdField[T] {
  def keyword:String
  def render(prefix:String) = prefix+keyword
}
case object IdJsonLdField extends KeywordLdField[NodeId] {
  def keyword:String = JsonLdKeywords.id
}
case object TypeJsonLdField extends KeywordLdField[Set[NodeId]] {
  def keyword:String = JsonLdKeywords.ldtype
}

sealed abstract class ElementsJsonLdField(val keyword:String) extends KeywordLdField[Seq[JsonLdModel]]

case object ListJsonLdField extends ElementsJsonLdField(JsonLdKeywords.list) 
case object SetJsonLdField extends ElementsJsonLdField(JsonLdKeywords.set) 
case object GraphJsonLdField extends ElementsJsonLdField(JsonLdKeywords.graph) 

case object ReverseJsonLdField extends KeywordLdField[LdObject] {
  def keyword:String =  JsonLdKeywords.reverse
} 
case object LanguageJsonLdField extends KeywordLdField[Language] {
  def keyword:String =  JsonLdKeywords.language
} 
case object ValueJsonLdField extends KeywordLdField[Any] {
  def keyword:String =  JsonLdKeywords.value
} 
case object IndexJsonLdField extends KeywordLdField[String] {
  def keyword:String =  JsonLdKeywords.index
} 

object JsonLdField {
  private val k = JsonLdKeywords

  private lazy val keywordsSet:Set[KeywordLdField[_]] = Set(IdJsonLdField, TypeJsonLdField, ReverseJsonLdField,
      ValueJsonLdField, LanguageJsonLdField, IndexJsonLdField, 
      GraphJsonLdField, ListJsonLdField, SetJsonLdField)
  
  def keyword(k:String):Option[KeywordLdField[_]] = keywordsSet.find(kw => kw.keyword == k)
  
}

object JsonLdFieldValue {

  def show[T](f:JsonLdField[T], v:T) = f match {
    case LdField(path) => path+":["+v.mkString(", ")+"]"
    case SetJsonLdField => s"@set :["+v.mkString(", ")+"]"
    case ListJsonLdField => s"@list :["+v.mkString(", ")+"]"
    case GraphJsonLdField => s"@graph :["+v.mkString(", ")+"]"
    case kf:KeywordLdField[_] => s"@${kf.keyword}"+":"+v
  }
  
}

case class JsonLdFieldValue[T](field:JsonLdField[T], value:T) {
  
  override def toString:String = {
    JsonLdFieldValue.show(field,value)
  }
}

object LdObject {
  
  class DuplicatedIdException(val fields:Seq[JsonLdFieldValue[_]]) extends Exception
  class DuplicatedLanguageException(val fields:Seq[JsonLdFieldValue[_]]) extends Exception
  class DuplicatedIndexException(val fields:Seq[JsonLdFieldValue[_]]) extends Exception
  class DuplicatedValueException(val fields:Seq[JsonLdFieldValue[_]]) extends Exception
  class InvalidKeywordException(val keyword:String) extends Exception
  
  def groupFields(fields:Seq[JsonLdFieldValue[_]]):Seq[JsonLdFieldValue[_]] = {
    var resRev = Buffer.empty[JsonLdFieldValue[_]] 
    
    var id:Option[NodeId] = None
    var lang:Option[Language] = None
    var ldValue:Option[Any] = None
    var ldIndex:Option[String] = None
    val typLst = collection.mutable.Set.empty[NodeId]
    val elems = collection.mutable.ListMap.empty[String, Seq[JsonLdModel]]
    val pathFields = collection.mutable.ListMap.empty[Path, Seq[JsonLdModel]]
    
    val revF = fields.foreach({
      case f @ JsonLdFieldValue(IdJsonLdField, obj) => {
        id match {
          case None => id = Some(obj)
          case Some(_) => throw new DuplicatedIdException(fields)
        }
      }
      case f @ JsonLdFieldValue(LanguageJsonLdField, obj) => {
        lang match {
          case None => lang = Some(obj)
          case Some(_) => throw new DuplicatedLanguageException(fields)
        }
      }
      case f @ JsonLdFieldValue(ValueJsonLdField, obj) => {
        ldValue match {
          case None => ldValue = Some(obj)
          case Some(_) => throw new DuplicatedValueException(fields)
        }
      }
      case f @ JsonLdFieldValue(IndexJsonLdField, obj) => {
        ldIndex match {
          case None => ldIndex = Some(obj)
          case Some(_) => throw new DuplicatedIndexException(fields)
        }
      }
      case f @ JsonLdFieldValue(TypeJsonLdField, obj) => {
        typLst ++= obj
      }
      case f @ JsonLdFieldValue(elsFld:ElementsJsonLdField, els) => {
        val k = elsFld.keyword
        elems.get(k) match {
          case None => elems += (k -> els)
          case Some(vs) => elems += (k -> (vs ++ els))
        }
      }
      case f @ JsonLdFieldValue(LdField(path), els) => {
        pathFields.get(path) match {
          case None => pathFields += (path -> els)
          case Some(vs) => pathFields += (path -> (vs ++ els))
        }
      }
      case f @ JsonLdFieldValue(ReverseJsonLdField, obj) => {
        resRev ++= obj.fields
      } 
    })
    
    id.map(JsonLdFieldValue(IdJsonLdField, _)).toSeq  ++
      (if (typLst.isEmpty) Nil else List(JsonLdFieldValue(TypeJsonLdField, typLst.toSet))) ++
      ldValue.map(JsonLdFieldValue(ValueJsonLdField, _)).toSeq ++
      lang.map(JsonLdFieldValue(LanguageJsonLdField, _)).toSeq ++
      ldIndex.map(JsonLdFieldValue(IndexJsonLdField, _)).toSeq ++
      elems.map { p =>
        p._1 match {
          case JsonLdKeywords.set => JsonLdFieldValue(SetJsonLdField, p._2)
          case JsonLdKeywords.list => JsonLdFieldValue(ListJsonLdField, p._2)
          case JsonLdKeywords.graph => JsonLdFieldValue(GraphJsonLdField, p._2)
          case x => throw new InvalidKeywordException(x)
        }
      } ++
      pathFields.map(p=> JsonLdFieldValue(LdField(p._1), p._2)) ++
      (if (resRev.isEmpty) Nil else Seq(JsonLdFieldValue(ReverseJsonLdField, new LdObject(resRev))))
    
  }
  
  def apply(fields:Seq[JsonLdFieldValue[_]]) = 
    new LdObject(groupFields(fields))
}

class LdObject private (override val fields:Seq[JsonLdFieldValue[_]]) extends JsonLdModel {

  lazy val id:Option[NodeId] = getField(IdJsonLdField)
  lazy val ldtype:Set[NodeId] = getField(TypeJsonLdField).getOrElse(Set.empty)

  lazy val index:Option[String] = getField(IndexJsonLdField)
  lazy val language:Option[Language] = getField(LanguageJsonLdField)
  
  
  lazy val keys:Seq[Path] = fields.collect(_.field match {
    case LdField(path) => path
  })
  
  def get(key:Path):Option[Seq[JsonLdModel]] = getField(LdField(key))
  lazy val reverse:Option[LdObject] = getField(ReverseJsonLdField)

  lazy val graph:Option[Seq[JsonLdModel]] = getField(GraphJsonLdField)
  
}

case class LdContainerKind private (property:String)

object LdContainerKind {
  private val k = JsonLdKeywords
  
  val set = LdContainerKind(k.set)
  val list = LdContainerKind(k.list)
  
}

/**
 * ref: file:///D:/docs/xml/specs/JSON-LD%201.0.htm#lists-and-sets#lists-and-sets
 * 
 * sembra che 
 * 
 * id:Option[NodeId] 
 * ldtype:Set[String] 
 * language:Option[Language]
 * 
 * non siano validi a livello di specifica (capire come mai vanoo bene al playground)
 */
case class LdContainer(containerKind:LdContainerKind, elements:Seq[JsonLdModel], index:Option[String]) extends JsonLdModel {
  def language:Option[Language] = None
  def id:Option[NodeId] = None
  def ldtype:Set[NodeId] = Set.empty
  
  override def fields:Seq[JsonLdFieldValue[_]] = {
    super.fields ++ Seq(containerKind match {
      case knd if knd == LdContainerKind.set => JsonLdFieldValue(SetJsonLdField, elements)
      case knd if knd == LdContainerKind.list => JsonLdFieldValue(ListJsonLdField, elements)
    })
  }
} 

sealed trait LdPrimitive extends JsonLdModel {
  def language:Option[Language]
  def value:Any
  def id:Option[NodeId] = None
  
  private val F = JsonLdField
  
  override def fields:Seq[JsonLdFieldValue[_]] = {
    super.fields ++ Seq(JsonLdFieldValue(ValueJsonLdField, value))
  }
}

object LdString {
  def apply(s:String):LdString = LdString(Set.empty, s, None, None)
}

object LdBooelan {
  def apply(s:Boolean):LdBoolean = LdBoolean(Set.empty, s, None, None)
}

object LdNumber {
  def apply(s:Double):LdNumber = LdNumber(Set.empty, s, None, None)
}

case class LdString(ldtype:Set[NodeId], value:String, index:Option[String], language:Option[Language]) extends LdPrimitive
case class LdBoolean(ldtype:Set[NodeId], value:Boolean, index:Option[String], language:Option[Language]) extends LdPrimitive
case class LdNumber(ldtype:Set[NodeId], value:Double, index:Option[String], language:Option[Language]) extends LdPrimitive

