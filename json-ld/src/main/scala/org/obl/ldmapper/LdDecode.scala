package org.obl.ldmapper

import org.obl.raz.Path
import scalaz.{ -\/, \/, \/- }

trait LdDecode[T] {
  def decode(j: JsonLdModel): String \/ T

  private def container(ck: LdContainerKind): LdDecode[Seq[T]] = {
    LdDecode.partial[Seq[T]]("json ld " + ck, {
      case LdContainer(kind, lst, _) => {
        Util.rightValueSeq(lst.map(this.decode))
      }
    })
  }

  def orElse(othr: => LdDecode[T]): LdDecode[T] = {
    val self = this
    LdDecode[T] { mdl: JsonLdModel =>
      self.decode(mdl) match {
        case r @ \/-(_) => r
        case els => {
          othr.decode(mdl)
        }
      }
    }
  }

  def list = container(LdContainerKind.list)
  def set = container(LdContainerKind.set)

  def map[T1](f: T => T1): LdDecode[T1] = LdDecode[T1]((mdl: JsonLdModel) => decode(mdl).map(f))

  def flatMap[T1](f: T => LdDecode[T1]): LdDecode[T1] = LdDecode[T1]((mdl: JsonLdModel) => decode(mdl).flatMap(itm => {
    f.apply(itm).decode(mdl)
  }))

}

object LdDecode extends LdDecodes {
  def apply[T](f: JsonLdModel => String \/ T) = {
    new LdDecode[T] {
      def decode(j: JsonLdModel): String \/ T = f(j)
    }
  }

  def partial[T](desc: String, pf: PartialFunction[JsonLdModel, String \/ T]) = {
    LdDecode[T] { v: JsonLdModel =>
      if (pf.isDefinedAt(v)) pf(v)
      else -\/("cant convert " + v + "to " + desc)
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
      } getOrElse -\/(s"invalid path $str")
    }
  }

  val link = path.orElse(LdDecode[Path] { jsonLdModel: JsonLdModel =>
    jsonLdModel.obj.flatMap { obj =>
      obj.id.map { id =>
        path.decode(LdString(Set.empty, id.render, None, None))
      }
    } getOrElse -\/(s"invalid path $jsonLdModel")
  })

  val nodeId = string.flatMap { str =>
    LdDecode[NodeId] { mdl: JsonLdModel =>
      UriParseUtil.parseNodeId(str).map { pth =>
        \/-(pth)
      } getOrElse -\/(s"invalid path $str")
    }
  }

  val nodeIdLink = nodeId.orElse(LdDecode[NodeId] { jsonLdModel: JsonLdModel =>
    jsonLdModel.obj.flatMap { obj =>
      obj.id.map { id =>
        nodeId.decode(LdString(Set.empty, id.render, None, None))
      }
    } getOrElse -\/(s"invalid path $jsonLdModel")
  })

  def enum[E <: Enumeration](e: E) = {
    partial[E#Value](e.toString, {
      case LdString(_, v, _, _) => \/.fromTryCatch({
        e.values.find(itm => itm == v).getOrElse(throw new Exception(v + "is not an " + e.toString))
      }).leftMap(_.getMessage())
    })
  }

  def withError[T](f: String => String)(implicit ld: LdDecode[T]) = {
    LdDecode[T] { mdl: JsonLdModel =>
      ld.decode(mdl).leftMap(f)
    }
  }

  def either[T1, T2](d1: LdDecode[T1], d2: LdDecode[T2]):LdDecode[T1 \/ T2] = {
    LdDecode[T1 \/ T2] { mdl: JsonLdModel =>
      d1.decode(mdl) match {
        case -\/(err1) => d2.decode(mdl) match {
          case \/-(v) => \/-(\/-(v))
          case -\/(err2) => -\/(s"Either error: (1) $err1, (2) $err2")
        }
        case \/-(v) => \/-(-\/(v))
      }
    }  
  }
}

trait LdFieldDecode[T] {
  type F
  def field: JsonLdField[F]
  protected def decode(j: Option[F]): String \/ T

  def apply(js: JsonLdModel): String \/ T = {
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
  def apply[F1, T](fld: JsonLdField[F1], f: Option[F1] => String \/ T) =
    new GenericLdFieldDecode[F1, T] {
      val field: JsonLdField[F] = fld
      def decode(j: Option[F]): String \/ T = f(j)
    }

  def jsonldField[T](fld: JsonLdField[T]): LdFieldDecode[T] =
    apply(fld, (v: Option[T]) => v match {
      case None => -\/("cant find " + fld)
      case Some(nid) => \/-(nid)
    })

  def tryDecode[F1, T](fld: JsonLdField[F1], f: F1 => String \/ T): GenericLdFieldDecode[F1, T] = {
    apply(fld, (v: Option[F1]) => v match {
      case None => -\/("cant find " + fld)
      case Some(nid) => f(nid)
    })
  }

  def mandatory[F1, T](fld: JsonLdField[F1], f: F1 => T): GenericLdFieldDecode[F1, T] =
    tryDecode(fld, v => \/.fromTryCatch(f(v)).leftMap(_.getMessage))

  def set[T](fld: Path)(implicit dec: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], Seq[T]] = {
    LdFieldDecode[Seq[JsonLdModel], Seq[T]](LdField(fld), { optf: Option[Seq[JsonLdModel]] =>
      optf match {
        case Some(vs) => {
          val r1:Seq[scalaz.\/[String,Seq[T]]]  = vs.map {
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
        case x => -\/("cant find field " + fld)
      }
    })
  }
  
  def list[T](fld: Path)(implicit dec: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], Seq[T]] =
    apply(fld)(dec.list)

  def apply[T](fld: Path)(implicit dec: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], T] = {
    singleElement[T](LdField(fld))(LdDecode.withError[T](verr => s"Error reading property $fld: $verr"))
  }

  def singleElement[T](fld: JsonLdField[Seq[JsonLdModel]])(implicit dec1: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], T] = {
    val dec = LdDecode.withError[T](verr => s"Error reading field $fld: $verr")
    LdFieldDecode[Seq[JsonLdModel], T](fld, { optf: Option[Seq[JsonLdModel]] =>
      optf match {
        case Some(v) if v.length == 1 => dec.decode(v.head)
        case Some(v) => -\/(s"expecting only an element got $v")
        case x => -\/("cant find field " + fld)
      }
    })
  }
  
  def nodeId: GenericLdFieldDecode[NodeId, NodeId] = mandatory(IdJsonLdField, (id: NodeId) => id)

  def id: GenericLdFieldDecode[NodeId, Path] = tryDecode(IdJsonLdField, (id: NodeId) => id match {
    case PathNodeId(path) => \/-(path)
    case x => -\/("invalid @id " + id)
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

