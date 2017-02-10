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
import com.ialekseev.bob.run.http._
import scalaz._
import Scalaz._
import scalaz.effect.IO

trait Service extends WebhookHttpService {
  this: BaseCommand with BaseHttpService =>

  def serviceCommand(dirs: List[String] = List.empty): IoTry[Unit] = {

    def build(sources: List[InputSource]): IoTry[List[BuildFailed \/ Build]] = {
      sources.map(source => {
        for {
          built <- exec.build(source.content, source.vars)
          _ <- IoTry.success(showResult(source.path, source.content, built))
        } yield built
      }).sequenceU
    }

    def runService(builds: List[Build]): IoTry[Unit] = {
      for {
        context <- IoTry {
          implicit val system = ActorSystem()
          implicit val materializer = ActorMaterializer()
          implicit val executionContext = system.dispatcher
          (system, executionContext, Http().bindAndHandle(createRoute(builds), "localhost", 8080))
        }
        _ <- IoTry.successIO(show(s"The Service is online at http://localhost:8080/\nPress RETURN to stop..."))
        _ <- IoTry.successIO(read())
        _ <- IoTry {
          val system = context._1
          implicit val executionContext = context._2
          val bindingFuture = context._3
          bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
        }
      } yield ()
    }

    val targetDirectories = if (dirs.nonEmpty) dirs else List(defaultSourcesLocation)

    val result: IoTry[Unit] = readSources(targetDirectories).flatMap(sources => {
        for {
          _ <- IoTry.successIO(show("building..."))
          _ <- {
            build(sources).flatMap(builds => {
              builds.sequenceU match {
                case \/-(builds: List[Build]) => {
                  val duplicateNamespaces = builds.groupBy(_.analysisResult.namespace).collect {case (x, List(_,_,_*)) => x}.toVector
                  if (duplicateNamespaces.length > 0) IoTry.successIO(showError("Please use different names for the duplicate namespaces: " + duplicateNamespaces))
                  else runService(builds)
                }
                case -\/(_) => IoTry.successIO(showError("Please fix the failed sources before starting the service"))
              }
            })
          }
        } yield ()
    })

    result.swap.flatMap(errors => IoTry.successSwappedIO(showError("Problem while trying to process the source files", errors: _*))).swap
  }
}
