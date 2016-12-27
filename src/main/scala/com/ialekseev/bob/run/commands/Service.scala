package com.ialekseev.bob.run.commands

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.ialekseev.bob.http.WebhookHttpService
import com.ialekseev.bob.run.Command

import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO

trait Service extends WebhookHttpService {
  this: Command =>

  def serviceCommand(): IO[Unit] = {
    readSources(defaultBuildsLocation).run.flatMap(readDisj => {
      readDisj match {
        case \/-(sources) => {

          for {
            built <- sources.map(source => {
                IO(exec.build(source.content).unsafePerformSync).flatMap(b => showResult(source.path, source.content, b).map(_ => b))
              }).sequenceU

            _ <- {
              built.sequenceU match {
                case \/-(builds) => {
                  for {
                    context <- IO {
                      implicit val system = ActorSystem("my-system")
                      implicit val materializer = ActorMaterializer()
                      implicit val executionContext = system.dispatcher
                      (system, executionContext, Http().bindAndHandle(createRoute(builds), "localhost", 8080))
                    }
                    _ <- show(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
                    _ <- read()
                    _ <- IO {
                      val system = context._1
                      implicit val executionContext = context._2
                      val bindingFuture = context._3
                      bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
                    }
                  } yield ()
                }
                case -\/(failed) => show("Please fix the failed sources before: " + failed)
              }
            }
          } yield ()
        }
        case -\/(errors) => showError("Problem while trying to read the source files", errors: _*)
      }
    })
  }
}
