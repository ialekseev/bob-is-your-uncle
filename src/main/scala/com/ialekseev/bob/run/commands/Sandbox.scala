package com.ialekseev.bob.run.commands

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.ialekseev.bob.http.SandboxHttpService
import scalaz.effect.IO

trait Sandbox extends SandboxHttpService {
  this: Command =>

  /*def sandboxCommand() = {
    for {
      context <- IO {
        implicit val system = ActorSystem()
        implicit val materializer = ActorMaterializer()
        implicit val executionContext = system.dispatcher
        (system, executionContext, Http().bindAndHandle(route, "localhost", 8080))
      }
      _ <- show(s"The Sandbox is online at http://localhost:8080/\nPress RETURN to stop...")
      _ <- read()
      _ <- IO {
        val system = context._1
        implicit val executionContext = context._2
        val bindingFuture = context._3
        bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
      }
    } yield ()
  }*/
}
