package com.ialekseev.bob

import scalaz.effect.IO
import scalaz.concurrent.Task

package object run {
  case class InputSource(name: String, content: String)
  case class InputDir(path: String, sources: List[InputSource], vars: List[Variable[String]])

  implicit class IOWrapper[T](io: IO[T]){
    def toTask: Task[T] = Task.delay(io.unsafePerformIO())
  }
}
