package org.obl.ldmapper

import org.obl.raz.Path
import scalaz.{ -\/, \/, \/- }

object LdMerge {

  def merge(objs:Seq[JsonLdModel]): String \/ LdObject = {
    val oobjs = objs.collect({
      case o:LdObject => o
    })
    lazy val error = -\/(s"cant merge json ld elements ${objs.mkString("\n")}")
    
    if (oobjs.length == objs.length) { 
      mergeObjects(oobjs).flatMap(_ match {
	      case None => error
	      case Some(v) => \/-(v)
	    })
    } else 
      error
  }
  
  def mergeObjects(objs:Seq[LdObject]): String \/ Option[LdObject] = {
    def checkSameIfBoth[T](fld: JsonLdField[T]): String \/ Option[JsonLdFieldValue[T]] = {
      val ids:Seq[T] = objs.map( _.getField(fld) ).collect({
        case Some(x) => x
      })
      ids match {
        case lst if lst.isEmpty => \/-(None)
        case hd +: rest => {
          rest.find(i => i != hd).
          	map(i => -\/(s"${objs} contains a item with a different value for field $fld")).
          	getOrElse(\/-(Some(JsonLdFieldValue(fld, hd))))
        }
      }
    }

    def mergeTypes: String \/ JsonLdFieldValue[Set[NodeId]] = 
      \/-(JsonLdFieldValue(TypeJsonLdField, objs.flatMap(_.ldtype).toSet))

    def mergeElements(fld: JsonLdField[Seq[JsonLdModel]]): String \/ JsonLdFieldValue[Seq[JsonLdModel]] = {
      \/-(JsonLdFieldValue(fld, objs.flatMap(obj => obj.getField(fld).toSeq.flatten) ))
    }

    def mergeReverse: String \/ Option[JsonLdFieldValue[LdObject]] = {
      val rps = objs.map(_.getField(ReverseJsonLdField)).collect({
        case Some(x) => x
      })
      mergeObjects( rps ).map(v => v.map( v =>  JsonLdFieldValue(ReverseJsonLdField, v)))
    }

    def addResItem(resFields: collection.mutable.Buffer[(Path, collection.mutable.Buffer[JsonLdModel])],
      resFieldIndex: collection.mutable.Map[Path, Int], k: Path, v: Seq[JsonLdModel]) = {
      resFieldIndex.get(k) match {
        case None => {
          var len = resFields.length
          resFields += (k -> v.toBuffer)
          resFieldIndex += (k -> len)
        }
        case Some(idx) => {
          resFields(idx)._2 ++= v
        }
      }
    }
    
    if (objs.isEmpty) \/-(None)
    else {
	    for (
	      id <- checkSameIfBoth(IdJsonLdField);
	      typ <- mergeTypes;
	      lang <- checkSameIfBoth(LanguageJsonLdField);
	      index <- checkSameIfBoth(IndexJsonLdField);
	      graph <- mergeElements(GraphJsonLdField)
	    ) yield {
	
	      val resFields = collection.mutable.Buffer.empty[(Path, collection.mutable.Buffer[JsonLdModel])]
	      val resFieldIndex = collection.mutable.Map.empty[Path, Int]
	    
	      objs.foreach { obj =>
	        obj.keys.foreach { k =>
	          obj.get(k).foreach { v =>
		          addResItem(resFields, resFieldIndex, k, v)
		        }
	        }
	      }
	      
        val grph = if (graph.value.isEmpty) Nil else Seq(graph)
        val ldtyp = if (typ.value.isEmpty) Nil else Seq(typ)
        
	      Some(LdObject(id.toSeq ++ ldtyp ++ lang.toSeq ++ index.toSeq ++ grph ++ resFields.map(p => JsonLdFieldValue(LdField(p._1), p._2))))
	    }
    }
  }

}

object LdMergeEncode {

  def merge[T1,T2](e1:LdEncode[T1], e2:LdEncode[T2]):LdEncode[(T1,T2)] = {
    LdEncode { t:(T1,T2) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2);
            r <- LdMerge.merge(Seq(o1, o2)) ) yield r
    }
  }
  
  def merge[T1,T2,T3](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3]):LdEncode[(T1,T2,T3)] = {
    LdEncode { t:(T1,T2,T3) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3);
            r <- LdMerge.merge(Seq(o1, o2, o3)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4]):LdEncode[(T1,T2,T3,T4)] = {
    LdEncode { t:(T1,T2,T3,T4) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5]):LdEncode[(T1,T2,T3,T4,T5)] = {
    LdEncode { t:(T1,T2,T3,T4,T5) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6]):LdEncode[(T1,T2,T3,T4,T5,T6)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7]):LdEncode[(T1,T2,T3,T4,T5,T6,T7)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14], e15:LdEncode[T15]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14); o15 <- e15.encode(t._15);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14], e15:LdEncode[T15], e16:LdEncode[T16]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14); o15 <- e15.encode(t._15); o16 <- e16.encode(t._16);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14], e15:LdEncode[T15], e16:LdEncode[T16], e17:LdEncode[T17]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14); o15 <- e15.encode(t._15); o16 <- e16.encode(t._16); o17 <- e17.encode(t._17);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14], e15:LdEncode[T15], e16:LdEncode[T16], e17:LdEncode[T17], e18:LdEncode[T18]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14); o15 <- e15.encode(t._15); o16 <- e16.encode(t._16); o17 <- e17.encode(t._17); o18 <- e18.encode(t._18);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14], e15:LdEncode[T15], e16:LdEncode[T16], e17:LdEncode[T17], e18:LdEncode[T18], e19:LdEncode[T19]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14); o15 <- e15.encode(t._15); o16 <- e16.encode(t._16); o17 <- e17.encode(t._17); o18 <- e18.encode(t._18); o19 <- e19.encode(t._19);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14], e15:LdEncode[T15], e16:LdEncode[T16], e17:LdEncode[T17], e18:LdEncode[T18], e19:LdEncode[T19], e20:LdEncode[T20]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14); o15 <- e15.encode(t._15); o16 <- e16.encode(t._16); o17 <- e17.encode(t._17); o18 <- e18.encode(t._18); o19 <- e19.encode(t._19); o20 <- e20.encode(t._20);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14], e15:LdEncode[T15], e16:LdEncode[T16], e17:LdEncode[T17], e18:LdEncode[T18], e19:LdEncode[T19], e20:LdEncode[T20], e21:LdEncode[T21]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14); o15 <- e15.encode(t._15); o16 <- e16.encode(t._16); o17 <- e17.encode(t._17); o18 <- e18.encode(t._18); o19 <- e19.encode(t._19); o20 <- e20.encode(t._20); o21 <- e21.encode(t._21);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20, o21)) ) yield r
    }
  }
  
  def merge[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](e1:LdEncode[T1], e2:LdEncode[T2], e3:LdEncode[T3], e4:LdEncode[T4], e5:LdEncode[T5], e6:LdEncode[T6], e7:LdEncode[T7], e8:LdEncode[T8], e9:LdEncode[T9], e10:LdEncode[T10], e11:LdEncode[T11], e12:LdEncode[T12], e13:LdEncode[T13], e14:LdEncode[T14], e15:LdEncode[T15], e16:LdEncode[T16], e17:LdEncode[T17], e18:LdEncode[T18], e19:LdEncode[T19], e20:LdEncode[T20], e21:LdEncode[T21], e22:LdEncode[T22]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)] = {
    LdEncode { t:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22) =>
      for ( o1 <- e1.encode(t._1); o2 <- e2.encode(t._2); o3 <- e3.encode(t._3); o4 <- e4.encode(t._4); o5 <- e5.encode(t._5); o6 <- e6.encode(t._6); o7 <- e7.encode(t._7); o8 <- e8.encode(t._8); o9 <- e9.encode(t._9); o10 <- e10.encode(t._10); o11 <- e11.encode(t._11); o12 <- e12.encode(t._12); o13 <- e13.encode(t._13); o14 <- e14.encode(t._14); o15 <- e15.encode(t._15); o16 <- e16.encode(t._16); o17 <- e17.encode(t._17); o18 <- e18.encode(t._18); o19 <- e19.encode(t._19); o20 <- e20.encode(t._20); o21 <- e21.encode(t._21); o22 <- e22.encode(t._22);
            r <- LdMerge.merge(Seq(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20, o21, o22)) ) yield r
    }
  }
  
}

