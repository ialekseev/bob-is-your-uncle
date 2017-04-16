package com.ialekseev.bob.run.cli

import java.nio.file.{Files, Path}
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import com.ialekseev.bob.run.TaskConversions._

trait Check {
  this: BaseCommand =>

  def checkCommand(filePath: Path): Task[Unit] = {

    val result: Task[Unit] = for {
      input <- ((readFile(filePath) |@| extractVarsForFile(filePath))((_, _)))
      built <- exec.build(input._1, input._2)
      _ <- showResult(filePath.toString, input._1, built).toTask
    } yield (): Unit

    result.handle {
      case e => showError("Can't read the file provided", e).toTask
    }
  }
}
