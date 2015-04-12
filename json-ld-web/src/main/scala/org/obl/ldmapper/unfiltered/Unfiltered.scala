package org.obl.ldmapper.unfiltered

import org.obl.ldmapper.web._
import unfiltered.request.HttpRequest
import unfiltered.request.Body
import org.obl.ldmapper.hydra.HttpMethod

object Unfiltered {

  implicit def bodyExtractor[REQ] = new BodyExtractor[HttpRequest[REQ]] {
  
    def body(req:HttpRequest[REQ]):String = Body.string(req)
    
  }
  
  implicit def reqMethodExtractor[REQ] = new RequestMethodExtractor[HttpRequest[REQ]] {
    def apply(req:HttpRequest[REQ]):HttpMethod.Value = {
      val mthd = req.method.toUpperCase
      HttpMethod.withName(mthd)
    }
  }
  
}