package com.ialekseev.bob.run.commands

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.ialekseev.bob.http.WebhookHttpService
import com.ialekseev.bob.run.Command
import scala.io.StdIn

trait Service extends WebhookHttpService {
  this: Command =>

  def serviceCommand() = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val builds = ??? //todo: compose builds

    val bindingFuture = Http().bindAndHandle(createRoute(builds), "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture.flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
