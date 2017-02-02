package com.ialekseev.bob.run.commands

import java.io.File
import scalaz._
import Scalaz._
import scalaz.effect.IO

trait Check {
  this: Command =>

  def checkCommand(filename: String): IO[Unit] = {
    require(filename.nonEmpty)

    (readFile(filename) |@| extractVarsForFile(filename))((_, _)).run.flatMap {
      case \/-((source, vars)) => showResult(filename, source, exec.build(source, vars))
      case -\/(errors) => showError("Can't read the file provided", errors : _*)
    }
  }
}
