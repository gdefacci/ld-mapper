package org.obl.ldmapper

import org.obl.raz.Path
import scalaz.{ -\/, \/, \/- }

import scala.language.higherKinds

trait LdDecode[T] {
  def decode(j: JsonLdModel): Throwable \/ T

  type LdDecodeSelf[T1] <: LdDecode[T1]
  def  decoderFactory[T1](f: JsonLdModel => Throwable \/ T1):LdDecodeSelf[T1]
  
  private def container(ck: LdContainerKind): LdDecodeSelf[Seq[T]] = {
    decoderFactory[Seq[T]] { v: JsonLdModel =>
      v match {
        case LdContainer(kind, lst, _) => Util.rightValueSeq(lst.map(this.decode))
        case _ => -\/(new LdDecode.LdDecodeException(v,"container"))
      }
    }
  }

  def orElse(othr: => LdDecode[T]): LdDecodeSelf[T] = {
    val self = this
    decoderFactory[T] { mdl: JsonLdModel =>
      self.decode(mdl) match {
        case r @ \/-(_) => r
        case els => {
          othr.decode(mdl)
        }
      }
    }
  }

//  def list = container(LdContainerKind.list)
//  def set = container(LdContainerKind.set)

  def map[T1](f: T => T1): LdDecodeSelf[T1] = decoderFactory[T1]((mdl: JsonLdModel) => decode(mdl).map(f))

  def flatMap[T1](f: T => LdDecode[T1]): LdDecodeSelf[T1] = decoderFactory[T1]((mdl: JsonLdModel) => decode(mdl).flatMap(itm => {
    f.apply(itm).decode(mdl)
  }))

}

object LdDecode extends LdDecodes {
  class LdDecodeException(from:Any, targetDescription:String) extends Exception(s"cant convert $from to $targetDescription")
  
  class BaseLdDecode {
    type LdDecodeSelf[T1] = LdDecode[T1]
    def  decoderFactory[T1](f: JsonLdModel => Throwable \/ T1):LdDecode[T1] = LdDecode(f)
  }
  
  def apply[T](f: JsonLdModel => Throwable \/ T) = {
    new BaseLdDecode with LdDecode[T] {
      def decode(j: JsonLdModel): Throwable \/ T = f(j)
    }
  }

  def partial[T](desc: String, pf: PartialFunction[JsonLdModel, Throwable \/ T]) = {
    LdDecode[T] { v: JsonLdModel =>
      if (pf.isDefinedAt(v)) pf(v)
      else -\/(new LdDecodeException(v,desc))
    }
  }

  implicit val boolean = partial[Boolean]("boolean", {
    case LdBoolean(_, v, _, _) => \/-(v)
  })
  implicit val int = partial[Int]("int", {
    case LdNumber(_, v, _, _) => \/-(v.intValue)
  })
  implicit val long = partial[Long]("long", {
    case LdNumber(_, v, _, _) => \/-(v.longValue)
  })
  implicit val double = partial[Double]("double", {
    case LdNumber(_, v, _, _) => \/-(v)
  })
  implicit val string = partial[String]("string", {
    case LdString(_, v, _, _) => \/-(v)
  })

  val path = string.flatMap { str =>
    LdDecode[Path] { mdl: JsonLdModel =>
      UriParseUtil.parseUrl(str).map { pth =>
        \/-(pth)
      } getOrElse -\/(new LdDecodeException(str, "url"))
    }
  }

  val link = path.orElse(LdDecode[Path] { jsonLdModel: JsonLdModel =>
    jsonLdModel.obj.flatMap { obj =>
      obj.id.map { id =>
        path.decode(LdString(Set.empty, id.render, None, None))
      }
    } getOrElse -\/(new LdDecodeException(jsonLdModel, "link"))
  })

  val nodeId = string.flatMap { str =>
    LdDecode[NodeId] { mdl: JsonLdModel =>
      UriParseUtil.parseNodeId(str).map { pth =>
        \/-(pth)
      } getOrElse -\/(new LdDecodeException(str, "link"))
    }
  }

  val nodeIdLink = nodeId.orElse(LdDecode[NodeId] { jsonLdModel: JsonLdModel =>
    jsonLdModel.obj.flatMap { obj =>
      obj.id.map { id =>
        nodeId.decode(LdString(Set.empty, id.render, None, None))
      }
    } getOrElse -\/(new LdDecodeException(jsonLdModel, "link"))
  })

  def enum[E <: Enumeration](e: E) = {
    partial[E#Value](e.toString, {
      case LdString(_, v, _, _) => \/.fromTryCatch({
        e.values.find(itm => itm == v).getOrElse(throw new Exception(v + "is not an " + e.toString))
      })
    })
  }

  def withError[T](f: Throwable => Throwable)(implicit ld: LdDecode[T]) = {
    LdDecode[T] { mdl: JsonLdModel =>
      ld.decode(mdl).leftMap(f)
    }
  }

  def either[T1, T2](d1: LdDecode[T1], d2: LdDecode[T2]):LdDecode[T1 \/ T2] = {
    LdDecode[T1 \/ T2] { mdl: JsonLdModel =>
      d1.decode(mdl) match {
        case -\/(err1) => d2.decode(mdl) match {
          case \/-(v) => \/-(\/-(v))
          case -\/(err2) => -\/(new LdDecodeException(mdl, "either"))
        }
        case \/-(v) => \/-(-\/(v))
      }
    }  
  }
  
  def container[T](ck: LdContainerKind)(implicit dec:LdDecode[T]): LdDecode[Seq[T]] = {
    LdDecode[Seq[T]] { v: JsonLdModel =>
      v match {
        case LdContainer(kind, lst, _) => Util.rightValueSeq(lst.map(dec.decode))
        case _ => -\/(new LdDecode.LdDecodeException(v,"container"))
      }
    }
  }
  
  def list[T](implicit dec:LdDecode[T]) = container[T](LdContainerKind.list)
  def set[T](implicit dec:LdDecode[T]) = container[T](LdContainerKind.set)
}

trait LdFieldDecode[T] {
  type F
  def field: JsonLdField[F]
  protected def decode(j: Option[F]): Throwable \/ T

  def apply(js: JsonLdModel): Throwable \/ T = {
    decode(js.getField(field))
  }

  def map[T1](f: T => T1): GenericLdFieldDecode[F, T1] =
    LdFieldDecode[F, T1](field, { o: Option[F] =>
      this.decode(o).map(f)
    })

  /**
   * not sure about
  def flatMap[T1](f: T => GenericLdFieldDecode[F, T1]): GenericLdFieldDecode[F, T1] = 
    LdFieldDecode[F, T1](field, (mdl: Option[F]) =>
      decode(mdl).flatMap { itm =>
        f.apply(itm).decode(mdl)
      }
    )
   */  

  def opt(nonecase: F => Boolean = f => false): GenericLdFieldDecode[F, Option[T]] =
    LdFieldDecode[F, Option[T]](field, {
      case None => \/-(None)
      case x @ Some(v) => {
        if (nonecase(v)) \/-(None)
        else decode(x).map(Some(_))
      }
    })

}

trait GenericLdFieldDecode[E, T] extends LdFieldDecode[T] {
  type F = E
}

object LdFieldDecode {
  
  class LdMissingFieldException(fld: JsonLdField[_]) extends Exception(s"$fld is missing")
  class SingleValueException(fld: JsonLdField[_], values:Seq[JsonLdModel]) extends Exception(s"$fld expecting a single value, got $values")
  class LdFieldException(fld: JsonLdField[_], cause:Throwable) extends Exception(s"error reding field $fld ", cause) 
  
  object LdFieldException {
    def apply(fld: JsonLdField[_], msg:String) = new LdFieldException(fld, new Exception(msg))
  }
  
  def apply[F1, T](fld: JsonLdField[F1], f: Option[F1] => Throwable \/ T) =
    new GenericLdFieldDecode[F1, T] {
      val field: JsonLdField[F] = fld
      def decode(j: Option[F]): Throwable \/ T = f(j)
    }

  def jsonldField[T](fld: JsonLdField[T]): LdFieldDecode[T] =
    apply(fld, (v: Option[T]) => v match {
      case None => -\/(new LdMissingFieldException(fld))
      case Some(nid) => \/-(nid)
    })

  def tryDecode[F1, T](fld: JsonLdField[F1], f: F1 => Throwable \/ T): GenericLdFieldDecode[F1, T] = {
    apply(fld, (v: Option[F1]) => v match {
      case None => -\/(new LdMissingFieldException(fld))
      case Some(nid) => f(nid)
    })
  }

  def mandatory[F1, T](fld: JsonLdField[F1], f: F1 => T): GenericLdFieldDecode[F1, T] =
    tryDecode(fld, v => \/.fromTryCatch(f(v)))

  def set[T](fld: Path)(implicit dec: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], Seq[T]] = {
    LdFieldDecode[Seq[JsonLdModel], Seq[T]](LdField(fld), { optf: Option[Seq[JsonLdModel]] =>
      optf match {
        case Some(vs) => {
          val r1:Seq[Throwable \/ Seq[T]]  = vs.map {
            case LdContainer(LdContainerKind.set, els, _) => {
              Util.rightValueSeq(els.map( v => dec.decode(v) ) )
            }
            case v => dec.decode(v).map(Seq(_))
          }
          val firstErr = r1.collectFirst {
            case e  @ -\/(_) => e
          }
          firstErr.getOrElse {
             \/-((r1.collect {
              case \/-(vs) => vs
            }).flatten)
          } 
        }
        case x => -\/(new LdMissingFieldException(LdField(fld)))
      }
    })
  }
  
  def list[T](fld: Path)(implicit dec: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], Seq[T]] =
    apply(fld)(LdDecode.list(dec))

  def apply[T](fld: Path)(implicit dec: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], T] = {
    singleElement[T](LdField(fld))(LdDecode.withError[T](verr => new LdFieldException(LdField(fld), verr))) //  s"Error reading property $fld: $verr"))
  }

  def singleElement[T](fld: JsonLdField[Seq[JsonLdModel]])(implicit dec1: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], T] = {
    val dec = LdDecode.withError[T](verr => new LdFieldException(fld, verr))
    LdFieldDecode[Seq[JsonLdModel], T](fld, { optf: Option[Seq[JsonLdModel]] =>
      optf match {
        case Some(v) if v.length == 1 => dec.decode(v.head)
        case Some(v) => -\/(new SingleValueException(fld, v))
        case x => -\/(new LdMissingFieldException(fld))
      }
    })
  }
  
  def nodeId: GenericLdFieldDecode[NodeId, NodeId] = mandatory(IdJsonLdField, (id: NodeId) => id)

  def id: GenericLdFieldDecode[NodeId, Path] = tryDecode(IdJsonLdField, (id: NodeId) => id match {
    case PathNodeId(path) => \/-(path)
    case x => -\/(new LdFieldException(IdJsonLdField, new LdDecode.LdDecodeException(x, "@id")))
  })

  def ldtype: GenericLdFieldDecode[Set[NodeId], Set[NodeId]] = mandatory(TypeJsonLdField, (id: Set[NodeId]) => id)

  def reverse[T](fld: Path)(implicit ec: LdDecode[T]): GenericLdFieldDecode[LdObject, T] = {
    reverse[T](LdFieldDecode(fld)(ec))
  }

  def reverse[T](fld: LdFieldDecode[T]): GenericLdFieldDecode[LdObject, T] = {
    val dec = LdDecode[T](fld)
    LdFieldDecode.tryDecode[LdObject, T](ReverseJsonLdField, dec.decode)
  }
}

