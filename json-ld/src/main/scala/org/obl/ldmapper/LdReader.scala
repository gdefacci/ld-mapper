package org.obl.ldmapper

import com.github.jsonldjava.utils.JsonUtils
import scalaz.{-\/, \/, \/-}
import com.github.jsonldjava.core.JsonLdOptions
import com.github.jsonldjava.core.JsonLdProcessor
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import Util.rightValueSeq

import collection.JavaConversions._

private[ldmapper] class LdObj(prefix:String, mp:Map[String,Any]) {
  private val Ld = JsonLdField

  lazy val keys:Iterable[JsonLdField[_]] = {
    mp.keys.map( k => 
      if (k.startsWith(prefix)) 
        JsonLdField.keyword(k.substring(prefix.length)).getOrElse(throw new Exception(s"invalid keyword $k")) 
      else 
        UriParseUtil.parseUrl(k).map( p => LdField(p)).getOrElse(throw new Exception(s"error getting property $k")) 
    )
  }
  
  lazy val entries:Iterable[(JsonLdField[_], Any)] = {
    mp.keys.map( k => { 
      var f:JsonLdField[_] = if (k.startsWith(prefix)) JsonLdField.keyword(k.substring(prefix.length)).get else UriParseUtil.parseUrl(k).map( p => LdField(p)).get 
      var v = mp.get(k).get
      f -> v
    })
  }
  
  private val k = JsonLdKeywords
  def key(str:String):String = prefix+str
  
  def id:Option[NodeId] = mp.get(key(k.id)).flatMap(a => { 
    UriParseUtil.parseNodeId(a.asInstanceOf[String]) 
  })
  def value:Option[Any] = mp.get(key(k.value))
  def reverse:Option[Any] = mp.get(key(k.reverse))
  
  def ldtype:Option[String \/ Seq[NodeId]] = mp.get(key(k.ldtype)).map(_.asInstanceOf[java.util.List[String]].toSeq).flatMap({
    case seq if seq.isEmpty => None
    case seq => Some(seq)
  } ).map { vs => 
    val vv:Seq[String \/ NodeId] = vs.map( (v:String) => UriParseUtil.parseNodeId(v) match {
    	case None => -\/("invalid NodeId "+v)
    	case Some(nd) => \/-(nd)
    } )
    Util.rightValueSeq(vv)
  }
  
  def language:Option[Language] = mp.get(key(k.language)).map( a => Language(a.asInstanceOf[String]))
  def index:Option[String] = mp.get(key(k.index)).map(_.asInstanceOf[String])
  
  def elements(k:String) = mp.get(key(k)).map(_.asInstanceOf[java.util.List[_]].toSeq)
  
  def list:Option[Seq[Any]] = elements(k.list)
  def set:Option[Seq[Any]] = elements(k.set)
  def graph:Option[Seq[Any]] = elements(k.graph)
  
  def get(str:String):Option[Seq[Any]] = mp.get(str).map(_.asInstanceOf[java.util.List[_]].toSeq)
}

private[ldmapper] class LdFieldValueFactory(toJsonLd:Any => String \/ JsonLdModel ) {
  private val KS = JsonLdKeywords
  private var F = JsonLdField 
  
  def apply(ld:JsonLdField[_], obj:LdObj):Option[String \/ JsonLdFieldValue[_]] = {
    
    ld match {
      case f @ IdJsonLdField => obj.id.map(v => \/-(JsonLdFieldValue(f, v)))
//      case f @ TypeJsonLdField => obj.ldtype.map(v => \/-(JsonLdFieldValue(f, v.toSet)))
      case f @ TypeJsonLdField => obj.ldtype.map( vvs => vvs.map( vs => JsonLdFieldValue(f, vs.toSet)))
      
      case f @ IndexJsonLdField => obj.index.map( v => \/-(JsonLdFieldValue(f, v)))
      case f @ LanguageJsonLdField => obj.language.map( v => \/-(JsonLdFieldValue(f, v)))
      case f :ElementsJsonLdField => {
        val k = f.keyword
        val objElems:Seq[String \/ JsonLdModel] = obj.elements(k).get.map(el => toJsonLd(el))
        val els:String \/ Seq[JsonLdModel] = Util.rightValueSeq( objElems )
        Some(els.map( els => JsonLdFieldValue(f, els)))
      }
      case f @ ReverseJsonLdField => obj.reverse.map( v => {
        toJsonLd(v).flatMap( (ldv:JsonLdModel) => ldv.fold(
            v => -\/("reverse cant be a primitive"), 
            v => \/-(JsonLdFieldValue(f, v)), 
            v => -\/("reverse cant be a container object"))) 
      })
      case f @ LdField(path) => {
//        Some(Util.rightValueSeq( obj.get(path.render).get.map(toJsonLd) ).map( v => JsonLdFieldValue(f,  v)))
        val pthTxt = path.render
        Some(Util.rightValueSeq( obj.get(pthTxt) match {
          case Some(vs) => vs.map(toJsonLd)
          case None => Seq(-\/("cant find field "+pthTxt))
        } ).map( v => JsonLdFieldValue(f,  v)))
      }
      case x => Some(-\/(s"unsupported json ld key:$x"))
    }
    
    
  }
}

object LdReader {
  
  def fromString(prefix:String = "@") = new LdReader[String](prefix, str =>  \/.fromTryCatch( JsonUtils.fromString(str) ).leftMap(_.getMessage()) )
  def fromReader(prefix:String = "@") = new LdReader[java.io.Reader](prefix, str =>  \/.fromTryCatch( JsonUtils.fromReader(str) ).leftMap(_.getMessage()) )
  
}

trait LdReadStrategy extends ((Any, Any => String \/ JsonLdModel ) => String \/ JsonLdModel)  

object LdReadStrategy {
  object Expanded extends LdReadStrategy {
    def apply(obj: Any, toJsonLdModel: Any => String \/ JsonLdModel): String \/ JsonLdModel = {
      Try {
        val context = new java.util.HashMap
        val options = new JsonLdOptions
        var compact = JsonLdProcessor.compact(obj, context, options)
        val arr = JsonLdProcessor.expand(compact)
        if (arr.isEmpty()) -\/(s"invalid json-ld document '$obj'")
        else {
          val jobj = arr.get(0)
          toJsonLdModel(jobj)
        }
      } match {
        case Success(v) => v
        case Failure(e) => {
          e.printStackTrace()
          -\/(e.getMessage)
        }
      }
    }
  }
  object Flattened extends LdReadStrategy {
    def apply(obj: Any, toJsonLdModel: Any => String \/ JsonLdModel): String \/ JsonLdModel = {
      Try {
        val options = new JsonLdOptions
        var res = JsonLdProcessor.flatten(obj, options)
        println(s">>>> $res")
        toJsonLdModel(res)
      } match {
        case Success(v) => v
        case Failure(e) => {
          e.printStackTrace()
          -\/(e.getMessage)
        }
      }
    }
  }
}

class LdReader[I](prefix:String, parser:I => (String \/ Object)) {

  private lazy val fieldValueFactory = new LdFieldValueFactory(toJsonLdModel)
  
  private def toJsonLdModel(obj:Any):String \/ JsonLdModel = {
    obj match {
      case mp:java.util.Map[_,_] => new LdObj(prefix, mp.toMap.asInstanceOf[Map[String, Any]]) match {
        case Primitive(m) => m
        case Container(c) => c
        case Obj(o) => o
        case mp => -\/(s"error converting $mp to jsonld")
      }
      case b:Boolean => \/-(LdBoolean(Set.empty, b, None, None ))
      case str:String => \/-( LdString(Set.empty, str, None, None ))
      case n:java.lang.Number => \/-(LdNumber(Set.empty, n.doubleValue, None, None))
      case lst:java.util.List[_]  =>  Util.rightValueSeq(lst.map(toJsonLdModel(_))).map( l1 => LdContainer(LdContainerKind.set, l1, None))
      case x => -\/(s"unexpected json ld :$x")
    }
  }

  private object Primitive {
    def unapply(mp: LdObj): Option[String \/ JsonLdModel] = {
      val vtyps: String \/ Seq[NodeId] = mp.ldtype match {
        case None => \/-(Seq.empty)
        case Some(v) => v
      }
      val v = mp.value
      mp.value.map { v =>
        vtyps.flatMap { typ =>
          v match {
            case b: Boolean => \/-(LdBoolean(typ.toSet, b, mp.index, mp.language))
            case str: String => \/-(LdString(typ.toSet, str, mp.index, mp.language))
            case n: java.lang.Number => \/-(LdNumber(typ.toSet, n.doubleValue, mp.index, mp.language))
            case x => -\/(s"unexpected primitve value :$x")
          }
        }
      }
    }
  }

  private object Container {
    def unapply(mp: LdObj): Option[String \/ JsonLdModel] = {
      val knd = mp.list.map(u => LdContainerKind.list).orElse(mp.set.map(u => LdContainerKind.set))
      val vtyps: String \/ Seq[NodeId] = mp.ldtype match {
        case None => \/-(Seq.empty)
        case Some(v) => v
      }
      mp.list.orElse(mp.set).map { (itms: Seq[Any]) =>
        vtyps.flatMap { typ: Seq[NodeId] =>
          rightValueSeq(itms.map(itm => toJsonLdModel(itm))).map( v => LdContainer(knd.get, v, mp.index))
        }
      }
    }
  }
  
  private object Obj {
    
    def unapply(mp:LdObj):Option[String \/ JsonLdModel] = {
      val flds = Util.rightValueSeq( mp.keys.flatMap { k => fieldValueFactory(k, mp) }.toSeq )
      Some(flds.map(LdObject.apply))
    }
    
  }
  
  def read(i:I, strategy:LdReadStrategy):String \/ JsonLdModel = {
    parser(i).flatMap(p => strategy(p, toJsonLdModel))
  }
  
  def decode[T](i:I, strategy:LdReadStrategy)(ld:LdDecode[T]):String \/ T = {
    read(i, strategy).flatMap(ld.decode(_))
  }
  
}