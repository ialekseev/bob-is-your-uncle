package com.ialekseev.bob.run.cli

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.ialekseev.bob.run.http.{BaseHttpService, SandboxHttpService}
import com.ialekseev.bob.run._
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO

trait Sandbox extends SandboxHttpService {
  this: BaseCommand with BaseHttpService =>

  def sandboxCommand(dir: Option[String] = none): Task[Unit] = {
    val targetDirectory = dir.getOrElse(defaultSourcesLocation)

    (for {
      context <- IO {
        implicit val system = ActorSystem()
        implicit val materializer = ActorMaterializer()
        implicit val executionContext = system.dispatcher
        (system, executionContext, Http().bindAndHandle(createRoutes(targetDirectory), "localhost", 8081)) //todo: move to the config
      }
      _ <- show(s"The Sandbox is online at http://localhost:8081/\nPress RETURN to stop...") //todo: from config
      _ <- read()
      _ <- IO {
        val system = context._1
        implicit val executionContext = context._2
        val bindingFuture = context._3
        bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
      }
    } yield ()).toTask
  }
}
