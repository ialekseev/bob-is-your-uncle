package com.ialekseev.bob.http

import akka.http.scaladsl.server.Route
import com.ialekseev.bob._
import com.ialekseev.bob.analyzer.Analyzer.Namespace
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.http.WebhookHttpService.{HttpResponseRun, HttpResponse}
import com.ialekseev.bob.{HttpRequest, HttpMethod}
import com.ialekseev.bob.exec.Executor
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.ext.EnumSerializer
import org.json4s.{ DefaultFormats, native }
import scalaz._
import Scalaz._

trait WebhookHttpService extends Json4sSupport {
  val exec: Executor

  implicit val formats = DefaultFormats + new EnumSerializer(HttpMethod)
  implicit val serialization = native.Serialization

  def createRoute(builds: Seq[Build]): Route = ctx => {
    require(builds.nonEmpty)

    val uri = ctx.request.uri.path.toString()
    val method = HttpMethod.withName(ctx.request.method.value)
    val headers = ctx.request.headers.map(h => (h.name, h.value)).toMap
    val queryString = ctx.request.uri.query().toMap
    val request = HttpRequest(some(uri), method, headers, queryString, none)
    val task = exec.run(request, builds).map(res => HttpResponse(request, res.runs.map(run => {
      println(Console.GREEN + s"[Done] ${run.build.analysisResult.namespace.path}#${run.build.analysisResult.namespace.name}: ${run.result}" + Console.RESET)
      HttpResponseRun(run.build.analysisResult.namespace, run.result)
    })))


    val future = unsafeToScala(task)
    ctx.complete(future)
  }
}

object WebhookHttpService {
  case class HttpResponse(incoming: HttpRequest, runs: Seq[HttpResponseRun])
  case class HttpResponseRun(namespace: Namespace, result: Any)
}
