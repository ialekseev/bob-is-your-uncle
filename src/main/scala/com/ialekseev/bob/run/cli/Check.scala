package com.ialekseev.bob.run.cli

import java.io.File
import scalaz._
import Scalaz._
import scalaz.effect.IO
import scalaz.concurrent.Task
import com.ialekseev.bob.IoTry
import com.ialekseev.bob.exec.Executor._
import com.ialekseev.bob.run._

trait Check {
  this: BaseCommand =>

  def checkCommand(filename: String): Task[Unit] = {
    require(filename.nonEmpty)

    val result: Task[Unit] = for {
      input <- ((readFile(filename) |@| extractVarsForFile(filename))((_, _))).toTask
      built <- exec.build(input._1, input._2)
      _ <- showResult(filename, input._1, built).toTask
    } yield (): Unit

    result.handle {
      case e => showError("Can't read the file provided", e).toTask
    }
  }
}
