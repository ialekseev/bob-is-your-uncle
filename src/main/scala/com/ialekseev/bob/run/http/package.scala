package com.ialekseev.bob.run

import com.ialekseev.bob._
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, onComplete}
import akka.http.scaladsl.server.{RequestContext, Route, RouteResult}
import org.json4s.ext.EnumNameSerializer
import org.json4s.{CustomSerializer, DefaultFormats, native}
import org.json4s.native.JsonMethods._
import org.json4s.JsonAST.{JField, JObject, JString}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Failure
import scalaz.concurrent.Task
import com.ialekseev.bob.run.TaskConversions._

package object http {
  trait BaseHttpService {
    implicit val executionContext: ExecutionContext

    def completeTask[T : ToResponseMarshaller](task: Task[T]): Route =  completeFuture(task.toFuture())

    def completeTask[T : ToResponseMarshaller](ctx: RequestContext, task: Task[T]): Future[RouteResult] = ctx.complete(task.toFuture())

    private def completeFuture[T : ToResponseMarshaller](future: Future[T]): Route =  {
      onComplete(future) {
        case util.Success(r) => complete(r)
        case Failure(ex) => complete((StatusCodes.InternalServerError, ex.getMessage))
      }
    }
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
