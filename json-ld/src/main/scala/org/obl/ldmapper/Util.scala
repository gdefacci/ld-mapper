package org.obl.ldmapper

import scalaz.{ -\/, \/, \/- }
import java.io.Closeable

object Util {

  def rightValueSeq[A, B](vss: Seq[A \/ B]): A \/ Seq[B] = {
    var it = vss.iterator
    val res = collection.mutable.Buffer.empty[B]
    while (it.hasNext) {
      val nxt = it.next()
      if (nxt.isLeft) return nxt.map(_ => throw new RuntimeException("cant happen"))
      else nxt.foreach(res += _)
    }
    \/-(res);
  }

  def use[T <: Closeable, R](resource: => T)(effect: T => R): R = {
    var res: T = null.asInstanceOf[T]
    try {
      res = resource
      effect(res)
    } finally {
      if (res != null) res.close()
    }
  }

  def disjFlatten[A,B](d:A \/ (A \/ B)):A \/ B = {
    d match {
      case v @ -\/(_) => v
      case \/-(v @ -\/(_)) => v
      case \/-(\/-(b)) => \/-(b)
    }
  }
}