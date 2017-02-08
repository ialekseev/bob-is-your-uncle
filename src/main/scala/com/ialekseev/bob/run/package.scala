package com.ialekseev.bob

import scala.util.Try
import scalaz.Scalaz._
import scalaz.effect.IO
import scalaz.{EitherT}

package object run {
  case class InputSource(path: String, content: String, vars: List[(String, String)])

  type IoTry[T] =  EitherT[IO, List[Throwable], T]
  object IoTry {
    def apply[T](t: T): IoTry[T] = EitherT.eitherT[IO, List[Throwable], T](IO(Try(t).toDisjunction.leftMap(List(_))))

    def success[T](t: T): IoTry[T] = EitherT.eitherT[IO, List[Throwable], T](IO(t.right))
    def failure[T](f: Throwable): IoTry[T] = EitherT.eitherT[IO, List[Throwable], T](IO(List(f).left))
  }
}
