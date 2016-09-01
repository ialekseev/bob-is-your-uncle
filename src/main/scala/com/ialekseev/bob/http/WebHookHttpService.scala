package com.ialekseev.bob.http

import akka.http.scaladsl.server.Route
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.{HttpRequest, HttpMethod}
import com.ialekseev.bob.exec.Executor
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{ DefaultFormats, native }
import scalaz._
import Scalaz._

trait WebhookHttpService extends Json4sSupport {
  val exec: Executor

  implicit val formats = DefaultFormats
  implicit val serialization = native.Serialization

  def createRoute(builds: Seq[Build]): Route = ctx => {
    require(builds.nonEmpty)

    val uri = ctx.request.uri.path.toString()
    val method = HttpMethod.withName(ctx.request.method.value)
    val request = HttpRequest(uri, method, Map.empty, Map.empty, none)
    val task = exec.run(request, builds).map(_.map(_.build.analysisResult.namespace))
    val future = unsafeToScala(task)
    ctx.complete(future)
  }
}
