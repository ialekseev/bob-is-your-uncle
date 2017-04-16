package com.ialekseev.bob.run.cli

import java.nio.file.Path
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.ialekseev.bob.run.http.WebhookHttpService
import com.ialekseev.bob.BuildFailed
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.run._
import com.ialekseev.bob.run.TaskConversions._
import com.ialekseev.bob.run.http._
import scalaz._
import Scalaz._
import scalaz.effect.IO
import scalaz.concurrent.Task

trait Service extends WebhookHttpService {
  this: BaseCommand with BaseHttpService =>

  def serviceCommand(dirs: List[Path] = List.empty): Task[Unit] = {

    def build(inputDirs: List[InputDir]): Task[List[BuildFailed \/ Build]] = {
      inputDirs.map(inputDir => {
        inputDir.sources.map(inputSource => {
          for {
            built <- exec.build(inputSource.content, inputDir.vars)
            _ <- showResult(inputSource.name, inputSource.content, built).toTask
          } yield built
        })
      }).flatten.sequenceU
    }

    def runService(dirs: List[Path], builds: List[Build]): IO[Unit] = {
      for {
        _ <- show(s"Using builds: ${builds.map(_.analysisResult.namespace).mkString("[", ",", "]")}")
        context <- IO {
          implicit val system = ActorSystem("service-system")
          implicit val materializer = ActorMaterializer()
          implicit val executionContext = system.dispatcher
          (system, executionContext, Http().bindAndHandle(createRoutes(dirs, builds), "localhost", 8080)) //todo: move to the config
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

    val result: Task[Unit] = readSources(targetDirectories).flatMap(inputDirs => {
        for {
          _ <- show("building...").toTask
          _ <- {
            build(inputDirs).flatMap(built => {
              val succeededBuilds = built.filter(_.isRight).map(_.toEither.right.get)
              val duplicateNamespaces = succeededBuilds.groupBy(_.analysisResult.namespace).collect {case (x, List(_,_,_*)) => x}.toVector
              if (duplicateNamespaces.length > 0) showError("Please use different names for the duplicate namespaces: " + duplicateNamespaces).toTask //todo: validation doesn't feel right here. Plus, where to check it after builds get updated?
              else runService(targetDirectories, succeededBuilds).toTask
            })
          }
        } yield ()
    })

    result.handle {
      case e => showError("Problem while trying to process the source files", e).toTask
    }
  }
}
