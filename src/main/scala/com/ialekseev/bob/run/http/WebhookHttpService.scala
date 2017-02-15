package com.ialekseev.bob.run.http

import akka.http.scaladsl.server.Route
import com.ialekseev.bob.exec.analyzer.Analyzer.Namespace
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.exec.Executor.{Build, FailedRun, SuccessfulRun}
import com.ialekseev.bob.run.http.WebhookHttpService.{HttpResponse, HttpResponseRun}
import com.ialekseev.bob.{HttpMethod, HttpRequest, _}
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.ext.EnumSerializer
import org.json4s.{Serialization, Formats, DefaultFormats, native}
import scalaz.Scalaz._

trait WebhookHttpService extends BaseHttpService with Json4sSupport {
  val exec: Executor

  implicit val formats = DefaultFormats + new EnumSerializer(HttpMethod) //todo: why not EnumNameSerializer?
  implicit val serialization = native.Serialization

  def createRoute(builds: List[Build]): Route = ctx => {
    require(builds.nonEmpty)

    //todo: potential blocks might happen here (when there are blocks in bob-files, like when using scalaj-http).
    //Probably we'd better not block the default dispatcher, and instead configure special "blocking dispatcher. see: http://stackoverflow.com/questions/34641861/akka-http-blocking-in-a-future-blocks-the-server"

    val uri = ctx.request.uri.path.toString
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
        case FailedRun(build, errors) => {
          println(Console.RED + s"[Errors] ${build.analysisResult.namespace.path}#${build.analysisResult.namespace.name}:" + Console.RESET + " " + errors)
          HttpResponseRun(build.analysisResult.namespace, false)
        }
      })
    })

    completeIO(ctx, res)
  }
}

object WebhookHttpService {
  case class HttpResponse(incoming: HttpRequest, runs: List[HttpResponseRun])
  case class HttpResponseRun(namespace: Namespace, succeed: Boolean)
}
