package com.ialekseev.bob.run.commands

import com.ialekseev.bob.run.Command

import scalaz._
import scalaz.effect.IO

trait Check {
  this: Command =>

  def checkCommand(filename: String): IO[Unit] = {
    readSource(filename).run.flatMap(readDisj => {
      readDisj match {
        case \/-(source) => showResult(filename, source, exec.build(source).unsafePerformSync)
        case -\/(error) => showError("Can't read the file provided", error)
      }
    })
  }
}
