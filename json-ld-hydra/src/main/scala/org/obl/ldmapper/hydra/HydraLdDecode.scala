package org.obl.ldmapper
package hydra

import scalaz.{-\/, \/, \/-}

object HydraLdDecode {

  implicit lazy val httpStatusCodeLdDecoder = LdDecode[HttpStatuses.Status] { json: JsonLdModel =>

    json.number.flatMap { ldn =>
      	val code = ldn.value.intValue
    	HttpStatuses.values.collectFirst({
          case st @ HttpStatuses.StatusImpl(cd1, _) if cd1 == code => \/-(st)
        })  
    } getOrElse -\/(new LdDecode.LdDecodeException(json, "http status"))
//  } getOrElse -\/(s"invalid HttpStatus: $json")
    
  }
  
  implicit lazy val httpMethodLdDecoder = LdDecode[HttpMethod.Value] { json: JsonLdModel =>
    json.string.
    	map( lds => \/.fromTryCatch( HttpMethod.withName(lds.value) ) ).
    	getOrElse( -\/(new LdDecode.LdDecodeException(json, "http method")) )
//    	getOrElse( -\/(s"invalid http method $json") )
  }
  
  
  
}