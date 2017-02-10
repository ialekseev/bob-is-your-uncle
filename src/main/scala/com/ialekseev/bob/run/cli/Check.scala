package com.ialekseev.bob.run.cli

import java.io.File
import scalaz._
import Scalaz._
import scalaz.effect.IO
import com.ialekseev.bob.IoTry
import com.ialekseev.bob.exec.Executor._

trait Check {
  this: BaseCommand =>

  def checkCommand(filename: String): IoTry[Unit] = {
    require(filename.nonEmpty)

    val result: IoTry[Unit] = for {
      (source, vars) <- (readFile(filename) |@| extractVarsForFile(filename))((_, _))
      built <- exec.build(source, vars)
      _ <- IoTry.successIO(showResult(filename, source, built))
    } yield (): Unit

    result.swap.flatMap(errors => IoTry.successSwappedIO(showError("Can't read the file provided", errors : _*))).swap
  }
}
