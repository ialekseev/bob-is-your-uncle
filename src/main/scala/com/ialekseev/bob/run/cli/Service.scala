package com.ialekseev.bob.run.cli

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.run.boot.HttpServiceUnsafe
import com.ialekseev.bob.run.http.WebhookHttpService
import com.ialekseev.bob.run.InputSource
import com.ialekseev.bob.BuildFailed
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.run._
import com.ialekseev.bob.run.http._
import scalaz._
import Scalaz._
import scalaz.effect.IO
import scalaz.concurrent.Task

trait Service extends WebhookHttpService {
  this: BaseCommand with BaseHttpService =>

  def serviceCommand(dirs: List[String] = List.empty): Task[Unit] = {

    def build(sources: List[InputSource]): Task[List[BuildFailed \/ Build]] = {
      sources.map(source => {
        for {
          built <- exec.build(source.content, source.vars)
          _ <- showResult(source.path, source.content, built).toTask
        } yield built
      }).sequenceU
    }

    def runService(builds: List[Build]): IO[Unit] = {
      for {
        context <- IO {
          implicit val system = ActorSystem("service-system")
          implicit val materializer = ActorMaterializer()
          implicit val executionContext = system.dispatcher
          (system, executionContext, Http().bindAndHandle(createRoute(builds), "localhost", 8080)) //todo: move to the config
        }
        _ <- show(s"The Service is online at http://localhost:8080/\nPress RETURN to stop...") //todo: from config
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

    val result: Task[Unit] = readSources(targetDirectories).flatMap(sources => {
        for {
          _ <- show("building...").toTask
          _ <- {
            build(sources).flatMap(builds => {
              builds.sequenceU match {
                case \/-(builds: List[Build]) => {
                  val duplicateNamespaces = builds.groupBy(_.analysisResult.namespace).collect {case (x, List(_,_,_*)) => x}.toVector
                  if (duplicateNamespaces.length > 0) showError("Please use different names for the duplicate namespaces: " + duplicateNamespaces).toTask
                  else runService(builds).toTask
                }
                case -\/(_) => showError("Please fix the failed sources before starting the service").toTask
              }
            })
          }
        } yield ()
    })

    result.handle {
      case e => showError("Problem while trying to process the source files", e).toTask
    }
  }
}
