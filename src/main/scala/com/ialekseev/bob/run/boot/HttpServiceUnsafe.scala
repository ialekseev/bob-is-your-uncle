package com.ialekseev.bob.run.boot

import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RequestContext, RouteResult, StandardRoute}
import scala.concurrent.Future
import scalaz.concurrent.Task
import delorean._

trait HttpServiceUnsafe {
  def completeTask[T : ToResponseMarshaller](task: Task[T]): StandardRoute = complete(task.unsafeToFuture())
  def completeTask[T : ToResponseMarshaller](ctx: RequestContext, task: Task[T]): Future[RouteResult] = ctx.complete(task.unsafeToFuture())
}
