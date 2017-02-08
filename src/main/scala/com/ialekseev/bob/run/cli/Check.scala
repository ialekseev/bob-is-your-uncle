package com.ialekseev.bob.run.cli

import java.io.File
import scalaz._
import Scalaz._
import scalaz.effect.IO

trait Check {
  this: BaseCommand =>

  def checkCommand(filename: String): IO[Unit] = {
    require(filename.nonEmpty)

    (readFile(filename) |@| extractVarsForFile(filename))((_, _)).run.flatMap {
      case \/-((source, vars)) => exec.build(source, vars).flatMap(showResult(filename, source, _))
      case -\/(errors) => showError("Can't read the file provided", errors : _*)
    }
  }
}
