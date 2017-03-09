package com.ialekseev.bob.run.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.exec.Executor.{Build, FailedRun, SuccessfulRun}
import com.ialekseev.bob.exec.analyzer.Analyzer.Namespace
import com.ialekseev.bob.run.http.WebhookHttpService.{HttpResponse, HttpResponseRun}
import com.ialekseev.bob.{HttpMethod, HttpRequest}
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import scalaz.Scalaz._

trait WebhookHttpService extends BaseHttpService with Json4sSupport {
  val exec: Executor

  val prefix = "hook"

  def createRoute(builds: List[Build]) = pathPrefix(prefix) {
    processingRoute(builds)
  }

  private def processingRoute(builds: List[Build]): Route = ctx => {
    require(builds.nonEmpty)

    val path = ctx.request.uri.path.toString
    val uri = path.substring(path.indexOf(prefix) + prefix.length)
    val method = HttpMethod.withName(ctx.request.method.value)
    val headers = ctx.request.headers.map(h => (h.name, h.value)).toMap
    val queryString = ctx.request.uri.query().toMap
    val request = HttpRequest(some(uri), method, headers, queryString, none)

    val res = exec.run(request, builds).map(r => {
      HttpResponse(request, r.runs.map {
        case SuccessfulRun(build, result) => {
          println(Console.GREEN + s"[Done] ${build.analysisResult.namespace.path}#${build.analysisResult.namespace.name}" + Console.RESET)
          HttpResponseRun(build.analysisResult.namespace, true)
        }
        case FailedRun(build) => {
          println(Console.RED + s"[Errors] ${build.analysisResult.namespace.path}#${build.analysisResult.namespace.name}:" + Console.RESET + " " + "failed")
          HttpResponseRun(build.analysisResult.namespace, false)
        }
      })
    })

    completeTask(ctx, res)
  }
}

object WebhookHttpService {
  case class HttpResponse(incoming: HttpRequest, runs: List[HttpResponseRun])
  case class HttpResponseRun(namespace: Namespace, succeed: Boolean)
}
