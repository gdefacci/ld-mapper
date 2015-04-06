package org.obl.ldmapper

import scalaz.{\/, \/-, -\/}
import org.obl.raz.Path
import scala.util.Try
import java.net.URLDecoder
import org.obl.raz.QParamSg
import org.obl.raz.PathSg
import org.obl.raz._

object UriParseUtil {

  private def decodeUrl(str: String) = {
    URLDecoder.decode(str.replace("+", "%2B"), "UTF-8").replace("%2B", "+").trim
  }

  private def jparseUrl(str: String): Option[java.net.URI] = {
    Try(new java.net.URI(decodeUrl(str))).toOption
  }

//  def parseNonFragmentUrl(str:String):Option[BasePath[PathPosition, ParamPosition]] = {
//    parseUrl(str) match {
//      case Path(None, pth, pars, None) => Some(RelativePath(pth, pars))
//      case Path(Some(base), pth, pars, None) => Some(AbsolutePath(base, pth, pars))
//    }
//  }
  
  def parseUrl(str: String): Option[Path] = {
    if (str.trim.length == 0) None
    else Try(new java.net.URI(decodeUrl(str))).toOption.map { uri =>
      val path = uri.getPath

      val base =
        if (uri.getScheme != null && uri.getSchemeSpecificPart != null) {
          uri.getScheme match {
            case "http" if (uri.getPort > 0) => Some(HTTP(uri.getHost, uri.getPort))            
            case "http" => Some(HTTP(uri.getHost))            
            case "https" if (uri.getPort > 0) => Some(HTTPS(uri.getHost, uri.getPort))
            case "https" => Some(HTTPS(uri.getHost))
            case _ => None
          }
        } else {
          None
        }

      val pth = if (path.startsWith("/")) path.substring(1) else path
      val pathParts = pth.split("/")

      val params: Seq[QParamSg] =
        if (uri.getQuery == null) {
          Nil
        } else {
          uri.getQuery().split(",|&").map { nmVal =>
            val nmVals: Array[String] = nmVal.split("=")
            val len = nmVals.length
            if (len == 1) {
              QParamSg(nmVals(0), None)
            } else if (len == 2) {
              QParamSg(nmVals(0), Some(nmVals(1)))
            } else {
              throw new RuntimeException("invalida query param part " + nmVal)
            }
          }
        }

      val fragment = Option(uri.getFragment)
      
      Path(base, PathSg(pathParts.toList), params, fragment)
    }
  }
  
  def parseNodeId(str:String):Option[NodeId] = {
    val nodeIdPrefix = "_:"
    if (str.startsWith(nodeIdPrefix)) Some(NodeId.blank(str.substring(nodeIdPrefix.length)))
    else parseUrl(str).map(NodeId(_))
  }
  
//  lazy val pathDecodeJsonLd:DecodeJsonLdValue[Path] = DecodeJsonLdValue.optionDecoder(_.string.flatMap(parseUrl(_)))
//  lazy val pathDecodeJsonLd:DecodeJsonLdValue[Path] = DecodeJsonLdValue.single( json => json.string.flatMap(parseUrl(_)) match {
//    case None => -\/(s"$json is not a valid url")
//    case Some(r) => \/-(r)
//  })
//  
//  lazy val nonFragmentPathDecodeJsonLd:DecodeJsonLdValue[BasePath[RelativePathAspect, NonFragmentPath, CanHavePrefixAspect]] = 
//    DecodeJsonLdValue.single( json => json.string.flatMap(parseNonFragmentUrl(_)) match {
//    case None => -\/(s"$json is not a non fragment url")
//    case Some(r) => \/-(r)
//  })
//  
//  
////  lazy val nodeIdDecodeJsonLd:DecodeJsonLdValue[NodeId] = DecodeJsonLdValue.optionDecoder(_.string.flatMap(parseNodeId(_)))
//  lazy val nodeIdDecodeJsonLd:DecodeJsonLdValue[NodeId] = DecodeJsonLdValue.single( json => json.string.flatMap(parseNodeId(_)) match {
//    case None => -\/(s"$json is not a valid NodeId")
//    case Some(r) => \/-(r)
//  })
 
  
//  lazy val optPathDecodeJsonLd:DecodeJsonLdValue[Option[Path]] = DecodeJsonLdValue.optionalWhen({ json =>
//    json.isNull || json.string.map(_.isEmpty).getOrElse(false)
//  })(pathDecodeJsonLd)
}