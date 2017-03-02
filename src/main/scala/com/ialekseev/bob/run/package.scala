package com.ialekseev.bob

import scalaz.effect.IO
import scalaz.concurrent.Task

package object run {
  case class InputSource(path: String, content: String, vars: List[(String, String)])

  implicit class IOWrapper[T](io: IO[T]){
    def toTask: Task[T] = Task.delay(io.unsafePerformIO())
  }
}
