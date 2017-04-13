package com.ialekseev.bob.run

import com.ialekseev.bob._
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.server.{RequestContext, Route, RouteResult}
import org.json4s.ext.EnumNameSerializer
import org.json4s.{CustomSerializer, DefaultFormats, native}
import org.json4s.native.JsonMethods._
import org.json4s.JsonAST.{JField, JObject, JString}
import scala.concurrent.{ExecutionContext, Future}
import scalaz.concurrent.Task

package object http {
  trait BaseHttpService {
    implicit val executionContext: ExecutionContext
    def completeTask[T : ToResponseMarshaller](task: Task[T]): Route
    def completeTask[T : ToResponseMarshaller](ctx: RequestContext, task: Task[T]): Future[RouteResult]
  }

  implicit val formats = DefaultFormats + new EnumNameSerializer(HttpMethod) + new BodySerializer
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
