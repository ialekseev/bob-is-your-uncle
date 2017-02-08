package com.ialekseev.bob.run

import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.server.{RouteResult, RequestContext, StandardRoute}
import scala.concurrent.Future
import scalaz.effect.IO

package object http {
  trait BaseHttpService {
    def completeIO[T : ToResponseMarshaller](ioTry: IoTry[T]): StandardRoute
    def completeIO[T : ToResponseMarshaller](ctx: RequestContext, io: IO[T]): Future[RouteResult]
  }
}
