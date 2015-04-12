package org.obl.ldmapper.unfiltered

import org.obl.raz.PathDecoder
import org.obl.ldmapper.hydra.Operation
import org.obl.ldmapper.web._
import scalaz.{ -\/, \/, \/- }
import unfiltered.response.ResponseFunction
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.HttpServletRequest
import unfiltered.request.HttpRequest

object UnfilteredAppOperation {

  type ActionType[P, I, O] = ImplReqModel[P, I, O, HttpRequest[HttpServletRequest]] => Throwable \/ ResponseFunction[HttpServletResponse]
  
  def apply[P, I, O](pathDecoder: PathDecoder[P], supportedOperation: Operation[I, O])(action: ActionType[P,I,O])(implicit resp: ResponseEncoder[ResponseFunction[HttpServletResponse]]) = {
    new ImplAppOperation[P,I,O,HttpRequest[HttpServletRequest], ResponseFunction[HttpServletResponse]](pathDecoder, supportedOperation)(action)(resp)    
  }

}