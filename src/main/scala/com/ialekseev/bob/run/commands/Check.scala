package com.ialekseev.bob.run.commands

import com.ialekseev.bob.run.Command
import scalaz._
import scalaz.effect.IO

trait Check {
  this: Command =>

  //todo: load external vars from the file's directory & pass them further
  def checkCommand(filename: String): IO[Unit] = {
    readSource(filename).run.flatMap {
      case \/-(source) => showResult(filename, source, exec.build(source).unsafePerformSync)
      case -\/(error) => showError("Can't read the file provided", error)
    }
  }
}
