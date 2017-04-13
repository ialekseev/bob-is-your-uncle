package com.ialekseev.bob.run.boot

import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RequestContext, Route, RouteResult}
import scalaz.concurrent.Task
import delorean._
import scala.concurrent.Future
import scala.util.Failure

trait HttpServiceUnsafe {
  def completeTask[T : ToResponseMarshaller](task: Task[T]): Route =  {
    onComplete(task.unsafeToFuture()) {
      case util.Success(r) => complete(r)
      case Failure(ex) => complete((StatusCodes.InternalServerError, ex.getMessage))
    }
  }

  def completeTask[T : ToResponseMarshaller](ctx: RequestContext, task: Task[T]): Future[RouteResult] = ctx.complete(task.unsafeToFuture())
}
