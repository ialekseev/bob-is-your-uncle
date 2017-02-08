package com.ialekseev.bob.run.boot

import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RequestContext, RouteResult, StandardRoute}
import com.ialekseev.bob.run._
import scala.concurrent.Future
import scalaz.effect.IO
import scalaz.{-\/, \/-}

trait HttpServiceUnsafe {

  def completeIO[T : ToResponseMarshaller](ioTry: IoTry[T]): StandardRoute = {
    ioTry.run.unsafePerformIO() match {
      case \/-(res) => complete(res)
      case -\/(errors) => complete(StatusCodes.InternalServerError) //todo: log
    }
  }

  def completeIO[T : ToResponseMarshaller](ctx: RequestContext, io: IO[T]): Future[RouteResult] = {
    ctx.complete(io.unsafePerformIO())
  }
}
