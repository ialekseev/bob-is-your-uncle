package com.ialekseev.bob.run.boot

import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RequestContext, RouteResult, StandardRoute}
import com.ialekseev.bob._
import scala.concurrent.Future
import scalaz.concurrent.Task
import scalaz.{-\/, \/-}
import delorean._

trait HttpServiceUnsafe {

  def completeTask[T : ToResponseMarshaller](task: Task[T]): StandardRoute = complete(task.unsafeToFuture())
  def completeTask[T : ToResponseMarshaller](ctx: RequestContext, task: Task[T]): Future[RouteResult] = ctx.complete(task.unsafeToFuture())

  def completeIO[T : ToResponseMarshaller](ioTry: IoTry[T]): StandardRoute = {
    ioTry.run.unsafePerformIO() match {
      case \/-(res) => complete(res)
      case -\/(errors) => complete(StatusCodes.InternalServerError) //todo: log
    }
  }
}
