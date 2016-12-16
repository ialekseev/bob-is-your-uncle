package com.ialekseev.bob.http

import akka.http.scaladsl.server.Route
import com.ialekseev.bob._
import com.ialekseev.bob.analyzer.Analyzer.Namespace
import com.ialekseev.bob.exec.Executor.{FailedRun, SuccessfulRun, Build}
import com.ialekseev.bob.http.WebhookHttpService.{HttpResponseRun, HttpResponse}
import com.ialekseev.bob.{HttpRequest, HttpMethod}
import com.ialekseev.bob.exec.Executor
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.ext.EnumSerializer
import org.json4s.{FieldSerializer, DefaultFormats, native}
import scalaz._
import Scalaz._

trait WebhookHttpService extends Json4sSupport {
  val exec: Executor

  implicit val formats = DefaultFormats + new EnumSerializer(HttpMethod) + FieldSerializer[HttpResponseRun]()
  implicit val serialization = native.Serialization

  def createRoute(builds: Seq[Build]): Route = ctx => {
    require(builds.nonEmpty)

    val uri = ctx.request.uri.path.toString()
    val method = HttpMethod.withName(ctx.request.method.value)
    val headers = ctx.request.headers.map(h => (h.name, h.value)).toMap
    val queryString = ctx.request.uri.query().toMap
    val request = HttpRequest(some(uri), method, headers, queryString, none)
    val task = exec.run(request, builds).map(res => HttpResponse(request, res.runs.map {
      case SuccessfulRun(build, result) => {
        println(Console.GREEN + s"[Done] ${build.analysisResult.namespace.path}#${build.analysisResult.namespace.name}" + Console.RESET)
        HttpResponseRun(build.analysisResult.namespace, some(result), none)
      }
      case FailedRun(build, error) => {
        println(Console.RED + s"[Error] ${build.analysisResult.namespace.path}#${build.analysisResult.namespace.name}:" + Console.RESET + " " + error.getMessage)
        HttpResponseRun(build.analysisResult.namespace, none, some(error.getMessage))
      }
    }))

    val future = unsafeToScala(task)
    ctx.complete(future)
  }
}

object WebhookHttpService {
  case class HttpResponse(incoming: HttpRequest, runs: Seq[HttpResponseRun])
  case class HttpResponseRun(namespace: Namespace, result: Option[Any], message: Option[String]) {
    val succeed = result.isDefined
  }
}
