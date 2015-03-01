package org.obl.jsonld

import org.obl.raz.Path
import scalaz.{ -\/, \/, \/- }

trait LdEncode[T] {

  def encode(t: T): String \/ JsonLdModel

  def contramap[T1](f: T1 => T) = LdEncode[T1]((t1: T1) => encode(f(t1)))

  private def container(knd: LdContainerKind): LdEncode[Seq[T]] =
    LdEncode[Seq[T]]((sq: Seq[T]) => Util.rightValueSeq(sq.map(encode)).map{ v => 
      LdContainer(knd, v, None)
    })

  def set: LdEncode[Seq[T]] = container(LdContainerKind.set)
  def list: LdEncode[Seq[T]] = container(LdContainerKind.list)
}

object LdEncode extends LdEncodes {

  def apply[T](f: T => String \/ JsonLdModel) =
    new LdEncode[T] {
      def encode(t: T): String \/ JsonLdModel = f(t)
    }

  def tryEncode[T](f: T => JsonLdModel) = LdEncode[T]((t: T) => \/.fromTryCatch(f(t)).leftMap(_.getMessage))

  implicit val boolean = LdEncode[Boolean] { b: Boolean => \/-(LdBoolean(Set.empty, b, None, None)) }
  implicit val int = LdEncode[Int] { i: Int => \/-(LdNumber(Set.empty, i, None, None)) }
  implicit val double = LdEncode[Double] { d: Double => \/-(LdNumber(Set.empty, d, None, None)) }
  implicit val string = LdEncode[String] { s: String => \/-(LdString(Set.empty, s, None, None)) }

  val path: LdEncode[Path] = string.contramap((p: Path) => p.render)
  val link: LdEncode[Path] = LdEncode[Path] { pth: Path =>
    \/-(LdObject(Seq(JsonLdFieldValue(IdJsonLdField, PathNodeId(pth)))))
  }

  val nodeId: LdEncode[NodeId] = string.contramap((p: NodeId) => p.render)
  val nodeIdLink: LdEncode[NodeId] = LdEncode[NodeId] { pth: NodeId =>
    \/-(LdObject(Seq(JsonLdFieldValue(IdJsonLdField, pth))))
  }

  def enum[E <: Enumeration] = LdEncode[E#Value] { (s: E#Value) => \/-(LdString(Set.empty, s.toString, None, None)) }
  def enumValue[E <: Enumeration#Value] = LdEncode[E] { (s: E) => \/-(LdString(Set.empty, s.toString, None, None)) }

  def append[T](ec: LdEncode[T], f: T => String \/ Seq[JsonLdFieldValue[_]]): LdEncode[T] = {
    LdEncode[T] { t: T =>
      ec.encode(t).flatMap { jsonModel: JsonLdModel =>
        f(t).map { flds =>
          if (flds.isEmpty) jsonModel
          else jsonModel.fold(
            leaf => LdObject(leaf.fields ++ flds),
            obj => LdObject(obj.fields ++ flds),
            container => LdObject(container.fields ++ flds))
        }
      }
    }
  }

  def prepend[T](f: T => String \/ Seq[JsonLdFieldValue[_]], ec: LdEncode[T]): LdEncode[T] = {
    LdEncode[T] { t: T =>
      ec.encode(t).flatMap { jsonModel: JsonLdModel =>
        f(t).map { flds =>
          jsonModel.fold(
            leaf => LdObject(flds ++ leaf.fields),
            obj => LdObject(flds ++ obj.fields),
            container => LdObject(flds ++ container.fields))
        }
      }
    }
  }

  def id[T](pth: NodeId, d: LdEncode[T]): LdEncode[T] = id[T]((t: T) => pth, d)

  def id[T](pth: T => NodeId, d: LdEncode[T]): LdEncode[T] = {
    prepend[T]((t: T) => \/-(Seq(JsonLdFieldValue(IdJsonLdField, pth(t)))), d)
  }

  def ldtype[T](pth: Set[NodeId], d: LdEncode[T]): LdEncode[T] = ldtype[T]((t: T) => pth, d)

  def ldtype[T](pth: T => Set[NodeId], d: LdEncode[T]): LdEncode[T] = {
    prepend[T]((t: T) => \/-(Seq(JsonLdFieldValue(TypeJsonLdField, pth(t)))), d)
  }

//  def set[T](implicit ec: LdEncode[T]): LdEncode[Seq[T]] = ec.set

  def linkTo[T](pathExtract: T => NodeId): LdEncode[T] = {
    LdEncode[T] { t: T =>
      val lnk: NodeId = pathExtract(t)
      \/-(LdObject(Seq[JsonLdFieldValue[_]](JsonLdFieldValue(IdJsonLdField, lnk))))
    }
  }

  def either[T1, T2](d1:LdEncode[T1], d2:LdEncode[T2]):LdEncode[T1\/ T2] = {
    LdEncode[T1 \/ T2] { (t:T1 \/ T2) =>
      t match {
        case -\/(v) => d1.encode(v)
        case \/-(v) => d2.encode(v)
      }
    }
  }
}

trait LdFieldEncode[T] {
  def encode(t: T): String \/ Seq[JsonLdFieldValue[_]]

  def contramap[T1](f: T1 => T) = LdFieldEncode[T1]((t1: T1) => encode(f(t1)))

  def opt = LdFieldEncode[Option[T]] { ov: Option[T] =>
    ov match {
      case None => \/-(Nil)
      case Some(v) => encode(v)
    }
  }

}

object LdFieldEncode {

  def apply[T](f: T => String \/ Seq[JsonLdFieldValue[_]]) = new LdFieldEncode[T] {
    def encode(t: T): String \/ Seq[JsonLdFieldValue[_]] = f(t)
  }

  def tryEncode[T](f: T => Seq[JsonLdFieldValue[_]]) = LdFieldEncode[T]((t: T) => \/.fromTryCatch(f(t)).leftMap(_.getMessage))

  def singleTryEncode[T](f: T => JsonLdFieldValue[_]) = tryEncode[T](t => Seq(f(t)))

  def jsonLdField[T](fld: JsonLdField[T]):LdFieldEncode[T] = tryEncode[T](v => Seq(JsonLdFieldValue(fld, v)))

  def id = nodeId.contramap[Path](NodeId.apply(_))

  def nodeId = jsonLdField(IdJsonLdField)

  def ldtype: LdFieldEncode[Set[NodeId]] = jsonLdField(TypeJsonLdField)

  def language = jsonLdField(LanguageJsonLdField)

  def index = jsonLdField(IndexJsonLdField)

  def reverse = jsonLdField(ReverseJsonLdField)
  
  def graph = jsonLdField(GraphJsonLdField)
  
  def list = jsonLdField(ListJsonLdField)
  
  def set = jsonLdField(SetJsonLdField)

//  def set[T](nd: Path)(implicit ec: LdEncode[T]): LdFieldEncode[Seq[T]] =
//    LdFieldEncode.apply[Seq[T]](nd)(ec.set)

  def set[T](nd:Path)(implicit ec:LdEncode[T]):LdFieldEncode[Seq[T]] =
    LdFieldEncode[Seq[T]]((ts:Seq[T]) => { 
      val r:String \/ Seq[JsonLdModel] = Util.rightValueSeq(ts.map { t =>
        ec.encode(t)
      })
      r.map( v => Seq(JsonLdFieldValue(LdField(nd), Seq(LdContainer(LdContainerKind.set, v, None)) )))
    })
  
  def list[T](nd: Path)(implicit ec: LdEncode[T]): LdFieldEncode[Seq[T]] = 
    LdFieldEncode.apply[Seq[T]](nd)(ec.list)

  def reverse[T](path: Path)(implicit ec: LdEncode[T]): LdFieldEncode[T] = reverse[T]((t: T) => path)(ec)

  def reverse[T](f: T => Path)(implicit ec: LdEncode[T]): LdFieldEncode[T] = {
    LdFieldEncode[T] { t: T =>
      ec.encode(t).map { rv: JsonLdModel =>
        val rvv = rv match {
          case LdContainer(_, elems, _) => elems
          case x => Seq(x)
        }
        Seq(JsonLdFieldValue(ReverseJsonLdField, LdObject(Seq(JsonLdFieldValue(LdField(f(t)), rvv)))))
      }
    }
  }

  def apply[T](nd: Path)(implicit ec: LdEncode[T]): LdFieldEncode[T] =
    LdFieldEncode[T]((t: T) => {
      val encv = ec.encode(t)
      encv.map((v: JsonLdModel) => {
        Seq(JsonLdFieldValue(LdField(nd), Seq(v)))
      })
    })

  
}