package com.ialekseev.bob

import scalaz.effect.IO
import scalaz.{-\/, \/-}
import scalaz.concurrent.Task

package object run {
  case class InputSource(path: String, content: String, vars: List[(String, String)])

  //todo: verify (see: http://timperrett.com/2014/07/20/scalaz-task-the-missing-documentation/)

  implicit class IOTryWrapper[T](ioTry: IoTry[T]){
    def toTask: Task[T] = {
      Task.delay(ioTry.run.unsafePerformIO()).flatMap {
        case \/-(result) => Task.now(result)
        case -\/(errors) => Task.fail(errors.head)
      }
    }
  }

  implicit class IOWrapper[T](io: IO[T]){
    def toTask: Task[T] = Task.delay(io.unsafePerformIO())
  }
}
