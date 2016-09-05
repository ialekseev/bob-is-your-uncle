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

  //todo: move to the config
  val defaultBuildsLocation = "playground"
  val fileExtension = ".bob"

  def serviceCommand() = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

     val sourcesTry = for {
      sourceFiles <- Try(new java.io.File(defaultBuildsLocation).listFiles.filter(_.getName.endsWith(fileExtension)))
      sources <- Try(sourceFiles.map(file => readSource(Source.fromFile(file)(Codec.UTF8)).get).toList)
    } yield sources

    sourcesTry match {
      case scala.util.Success(sources) => {

        val built: List[BuildFailed \/ Build] = sources.map(source => {
         val build = exec.build(source).unsafePerformSync
         showResult(source, build)
         build
       })

        built.sequenceU match {
          case \/-(builds) => {
            val bindingFuture = Http().bindAndHandle(createRoute(builds), "localhost", 8080)

            println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
            StdIn.readLine() // let it run until user presses return
            bindingFuture.flatMap(_.unbind()) // trigger unbinding from the port
              .onComplete(_ => system.terminate()) // and shutdown when done
          }
          case -\/(failed) => showError("Please fix the failed builds")
        }

      }
      case scala.util.Failure(e) => showError("Problem while trying to read the source files", e)
    }
  }
}
