package com.ialekseev.bob.run

import com.ialekseev.bob._
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.server.{RouteResult, RequestContext, StandardRoute}
import org.json4s.{native, DefaultFormats, CustomSerializer}
import org.json4s.native.JsonMethods._
import org.json4s.JsonAST.{JString, JField, JObject}
import scala.concurrent.Future
import scalaz.effect.IO

package object http {
  trait BaseHttpService {
    def completeIO[T : ToResponseMarshaller](ioTry: IoTry[T]): StandardRoute
    def completeIO[T : ToResponseMarshaller](ctx: RequestContext, io: IO[T]): Future[RouteResult]
  }

  implicit val formats = DefaultFormats
  implicit val serialization = native.Serialization
  class BodySerializer extends CustomSerializer[Body](format => (
    {
      case JObject(JField("text", JString(text)) :: Nil) => StringLiteralBody(text)
      case JObject(JField("dic", JObject(dic)) :: Nil) => DictionaryBody(dic.map(d => (d._1, d._2.extract[String])).toMap)
      case JObject(JField("json", JString(jsonStr)) :: Nil) => JsonBody(parse(jsonStr))
    },
    PartialFunction.empty
    ))
}
