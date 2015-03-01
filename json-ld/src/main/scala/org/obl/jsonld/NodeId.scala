package org.obl.jsonld

import org.obl.raz._
import scala.language.implicitConversions

sealed trait NodeId {
  def render:String
}

object NodeId {
  
  def blank(id:String) = new BlankNodeId(id)
  implicit def apply(pth:Path) = new PathNodeId(pth)
  
}

case class BlankNodeId(identifier:String) extends NodeId {
  def render = s"_:$identifier"
}
case class PathNodeId(path:Path) extends NodeId {
  def render = path.render
}
