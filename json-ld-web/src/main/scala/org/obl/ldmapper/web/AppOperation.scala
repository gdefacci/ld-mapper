package org.obl.ldmapper.web

import org.obl.raz.{ PathDecoder }
import org.obl.ldmapper.{ LdDecode, LdEncode }
import org.obl.ldmapper.hydra.Operation
import scalaz.{ -\/, \/, \/- }
import org.obl.ldmapper.LdReader
import org.obl.ldmapper.LdReadStrategy
import org.obl.ldmapper.LdReadOptions
import org.obl.ldmapper.LdPrintOptions
import org.obl.ldmapper.LdPrinter
import org.obl.ldmapper.hydra.HttpMethod

trait BodyExtractor[U] {

  def body(req: U): String

}

trait ResponseEncoder[R] {

  def apply(v: Throwable \/ String): R
}

trait RequestMethodExtractor[U] {
  def apply(req: U): HttpMethod.Value
}

case class ReqModel[P, I](path: P, body: I)
case class ImplReqModel[P, I, O, REQ](path: P, body: I, request:REQ, private val ldEncode:LdEncode[O]) {
  def valueOf[R](o:O)(implicit responseEncoder: ResponseEncoder[R], ldPrintOptions: LdPrintOptions):R = {
    responseEncoder(\/-( LdPrinter.print( ldEncode.encode(o) ) ))
  }
}

object AppOperation {
  def apply[P, I, O](pathDecoder: PathDecoder[P], supportedOperation: Operation[I, O])(action: ReqModel[P, I] => Throwable \/ O) = {
    new AppOperation[P, I, O](pathDecoder, supportedOperation)(action)
  }

//  def apply[P, I, O, REQ, R](pathDecoder: PathDecoder[P], supportedOperation: Operation[I, O])(action: ImplReqModel[P, I, O, REQ] => Throwable \/ R)(implicit responseEncoder: ResponseEncoder[R]) = {
//    new ImplAppOperation[P, I, O, REQ, R](pathDecoder, supportedOperation)(action)
//  }
}

class AppOperation[P, I, O](pathDecoder: PathDecoder[P], supportedOperation: Operation[I, O])(action: ReqModel[P, I] => Throwable \/ O) {

  def unapply[U, R](req: U)(
    implicit ldDecode: LdDecode[I], ldEncode: LdEncode[O],
    extPathDecode: org.obl.raz.ext.ExtPathDecode[U], bodyExtract: BodyExtractor[U], responseEncoder: ResponseEncoder[R], reqMethodExtractor: RequestMethodExtractor[U],
    ldReadOptions: LdReadOptions, ldPrintOptions: LdPrintOptions): Option[R] = {
    val mthd = reqMethodExtractor(req)

    if (mthd == supportedOperation.method) None
    else {
      val currPath = extPathDecode(req)
      pathDecoder.decodeFull(currPath).map { pth =>
        val content = bodyExtract.body(req)
        val r = LdReader.fromString(ldReadOptions).read(content).flatMap { jsnMdl =>
          ldDecode.decode(jsnMdl).flatMap { o: I =>
            val mdl = ReqModel(pth, o)
            action(mdl)
          }
        }
        responseEncoder(r.map(ldEncode.encode(_)).map(jsonMdl => LdPrinter.print(jsonMdl)))
      }.toOption
    }
  }

}

class ImplAppOperation[P, I, O, REQ, RESP](pathDecoder: PathDecoder[P], supportedOperation: Operation[I, O])(action: ImplReqModel[P, I, O, REQ] => Throwable \/ RESP)(implicit resp: ResponseEncoder[RESP]) {

  def unapply(req: REQ)(
    implicit ldDecode: LdDecode[I], ldEncode: LdEncode[O],
    extPathDecode: org.obl.raz.ext.ExtPathDecode[REQ], bodyExtract: BodyExtractor[REQ], reqMethodExtractor: RequestMethodExtractor[REQ],
    ldReadOptions: LdReadOptions, ldPrintOptions: LdPrintOptions): Option[RESP] = {
    val mthd = reqMethodExtractor(req)

    if (mthd == supportedOperation.method) None
    else {
      val currPath = extPathDecode(req)
      pathDecoder.decodeFull(currPath).flatMap { pth =>
        val content = bodyExtract.body(req)
        LdReader.fromString(ldReadOptions).read(content).flatMap { jsnMdl =>
          ldDecode.decode(jsnMdl).flatMap { o: I =>
            val mdl = ImplReqModel[P,I,O,REQ](pth, o, req, ldEncode)
            action(mdl)
          }
        }
      }.toOption
    }
  }
}