package com.ialekseev.bob.run.commands

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.ialekseev.bob.exec.Executor.{BuildFailed, Build}
import com.ialekseev.bob.http.WebhookHttpService
import com.ialekseev.bob.run.Command
import scala.io.{Codec, StdIn}
import scala.util.Try
import scala.io.Source
import scalaz._
import Scalaz._

trait Service extends WebhookHttpService {
  this: Command =>

  def serviceCommand() = {
    readSources(defaultBuildsLocation) match {
      case scala.util.Success(sources) => {

        val built: List[BuildFailed \/ Build] = sources.map(source => {
         val build = exec.build(source._2).unsafePerformSync
         showResult(source._1, source._2, build)
         build
       })

        built.sequenceU match {
          case \/-(builds) => {
            implicit val system = ActorSystem("my-system")
            implicit val materializer = ActorMaterializer()
            implicit val executionContext = system.dispatcher

            val bindingFuture = Http().bindAndHandle(createRoute(builds), "localhost", 8080)

            show(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
            read() // let it run until user presses return
            bindingFuture.flatMap(_.unbind()) // trigger unbinding from the port
              .onComplete(_ => system.terminate()) // and shutdown when done
          }
          case -\/(failed) => {
            show("Please fix the failed sources before\n")
          }
        }
      }
      case scala.util.Failure(e) => showError("Problem while trying to read the source files", e)
    }
  }
}
