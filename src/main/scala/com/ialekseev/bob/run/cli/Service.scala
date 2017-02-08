package com.ialekseev.bob.run.cli

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.run.boot.HttpServiceUnsafe
import com.ialekseev.bob.run.http.WebhookHttpService
import com.ialekseev.bob.run.InputSource
import com.ialekseev.bob.StageFailed
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.run.http._
import scalaz._
import Scalaz._
import scalaz.effect.IO

trait Service extends WebhookHttpService {
  this: BaseCommand with BaseHttpService =>

  def serviceCommand(dirs: List[String] = List.empty): IO[Unit] = {

    def build(sources: List[InputSource]): IO[List[StageFailed \/ Build]] = {
      sources.map(source => {
        exec.build(source.content, source.vars).flatMap(b => showResult(source.path, source.content, b).map(_ => b))
      }).sequenceU
    }

    def runService(builds: Seq[Build]): IO[Unit] = {
      for {
        context <- IO {
          implicit val system = ActorSystem()
          implicit val materializer = ActorMaterializer()
          implicit val executionContext = system.dispatcher
          (system, executionContext, Http().bindAndHandle(createRoute(builds), "localhost", 8080))
        }
        _ <- show(s"The Service is online at http://localhost:8080/\nPress RETURN to stop...")
        _ <- read()
        _ <- IO {
          val system = context._1
          implicit val executionContext = context._2
          val bindingFuture = context._3
          bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
        }
      } yield ()
    }

    val targetDirectories = if (dirs.nonEmpty) dirs else List(defaultSourcesLocation)

    readSources(targetDirectories).run.flatMap {
        case \/-(sources) => {
          for {
            _ <- show("building...")
            _ <- {
              build(sources).flatMap(builds => {
                builds.sequenceU match {
                  case \/-(builds: Seq[Build]) => {
                    val duplicateNamespaces = builds.groupBy(_.analysisResult.namespace).collect {case (x, List(_,_,_*)) => x}.toVector
                    if (duplicateNamespaces.length > 0) showError("Please use different names for the duplicate namespaces: " + duplicateNamespaces)
                    else runService(builds)
                  }
                  case -\/(_) => showError("Please fix the failed sources before starting the service")
                }
              })
            }
          } yield ()
        }
        case -\/(errors) => showError("Problem while trying to read the source files", errors: _*)
    }
  }
}
