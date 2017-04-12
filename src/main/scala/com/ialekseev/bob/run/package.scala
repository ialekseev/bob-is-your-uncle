package com.ialekseev.bob

import scalaz.effect.IO
import scalaz.concurrent.Task

package object run {
  case class InputSource(name: String, content: String)
  case class InputDir(path: String, sources: List[InputSource], vars: List[Variable[String]])
  case class ErrorCoordinates(x: Int, y: Int) {
    override def toString: String = s"($x, $y)"
  }

  implicit class IOWrapper[T](io: IO[T]){
    def toTask: Task[T] = Task.delay(io.unsafePerformIO())
  }

  def errorCoordinates(source: String, offset: Int): ErrorCoordinates = {
    require(offset >= 0)

    if (source.isEmpty || offset == 0) ErrorCoordinates(1, 1)
    else {
      val beforeOffset = source.take(offset)
      val nlIndex = beforeOffset.reverse.indexWhere(_ == '\n')

      val column = if (nlIndex >= 0) nlIndex + 1 else offset + 1
      val line = beforeOffset.count(_ == '\n') + 1
      ErrorCoordinates(line, column)
    }
  }
}
