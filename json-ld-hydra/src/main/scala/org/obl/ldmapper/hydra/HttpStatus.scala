package org.obl.ldmapper.hydra

import util.Try

object HttpStatuses extends Enumeration {

  trait Status extends Value { 
    def code: Int
  }

  case class StatusImpl(code: Int, name:String) extends Val(code, name) with Status

  private def status(code:Int, nm:String):Status = StatusImpl(code, nm)
  
  val continue = status(100,"continue")
  val switchingProtocols = status(101,"switchingProtocols")
  val processing = status(102,"processing")
  val ok = status(200,"ok")
  val created = status(201,"created")
  val accepted = status(202,"accepted")
  val nonAuthoritativeInformation = status(203,"nonAuthoritativeInformation")
  val noContent = status(204,"noContent")
  val resetContent = status(205,"resetContent")
  val partialContent = status(206,"partialContent")
  val multiStatus = status(207,"multiStatus")
  val alreadyReported = status(208,"alreadyReported")
  val iMUsed = status(226,"iMUsed")
  val multipleChoices = status(300,"multipleChoices")
  val movedPermanently = status(301,"movedPermanently")
  val found = status(302,"found")
  val seeOther = status(303,"seeOther")
  val notModified = status(304,"notModified")
  val useProxy = status(305,"useProxy")
  val temporaryRedirect = status(307,"temporaryRedirect")
  val badRequest = status(400,"badRequest")
  val unauthorized = status(401,"unauthorized")
  val paymentRequired = status(402,"paymentRequired")
  val forbidden = status(403,"forbidden")
  val notFound = status(404,"notFound")
  val methodNotAllowed = status(405,"methodNotAllowed")
  val notAcceptable = status(406,"notAcceptable")
  val proxyAuthenticationRequired = status(407,"proxyAuthenticationRequired")
  val requestTimeout = status(408,"requestTimeout")
  val conflict = status(409,"conflict")
  val gone = status(410,"gone")
  val lengthRequired = status(411,"lengthRequired")
  val preconditionFailed = status(412,"preconditionFailed")
  val requestEntityTooLarge = status(413,"requestEntityTooLarge")
  val requestURITooLong = status(414,"requestURITooLong")
  val unsupportedMediaType = status(415,"unsupportedMediaType")
  val requestedRangeNotSatisfiable = status(416,"requestedRangeNotSatisfiable")
  val expectationFailed = status(417,"expectationFailed")
  val teaPot = status(418,"teaPot")
  val tooManyConnections = status(421,"tooManyConnections")
  val unprocessableEntity = status(422,"unprocessableEntity")
  val locked = status(423,"locked")
  val failedDependency = status(424,"failedDependency")
  val unorderedCollection = status(425,"unorderedCollection")
  val updateRequired = status(426,"updateRequired")
  val preconditionRequired = status(428,"preconditionRequired")
  val tooManyRequests = status(429,"tooManyRequests")
  val requestHeaderFieldsTooLarge = status(431,"requestHeaderFieldsTooLarge")
  val internalServerError = status(500,"internalServerError")
  val notImplemented = status(501,"notImplemented")
  val badGateway = status(502,"badGateway")
  val serviceUnavailable = status(503,"serviceUnavailable")
  val gatewayTimeout = status(504,"gatewayTimeout")
  val versionNotSupported = status(505,"versionNotSupported")
  val variantAlsoNegotiates = status(506,"variantAlsoNegotiates")
  val insufficientStorage = status(507,"insufficientStorage")
  val loopDetected = status(508,"loopDetected")
  val notExtended = status(510,"notExtended")
  val networkAuthenticationRequired = status(511,"networkAuthenticationRequired")

  def valueOf(s:Int):Option[Status] = {
    values.collectFirst({
      case st:Status if st.code == s => {
        st
      }
    })
  }

}
