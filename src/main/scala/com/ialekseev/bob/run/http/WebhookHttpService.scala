package com.ialekseev.bob.run.http

import com.ialekseev.bob.exec.Executor.{FailedRun}
import com.ialekseev.bob.exec.analyzer.Analyzer.Namespace
import com.ialekseev.bob.run.{ErrorCoordinates, InputDir, errorCoordinates}
import com.ialekseev.bob.run.http.WebhookHttpService.{HttpResponse, HttpResponseRun}
import java.nio.file.Path
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.exec.Executor.{Build, RunResult, SuccessfulRun}
import com.ialekseev.bob.run._
import WebhookHttpService._
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import scalaz.concurrent.Task
import scalaz.syntax.either._
import scalaz.std.option._
import scalaz.{-\/, EitherT, \/, \/-}
import com.bfil.automapper._

trait WebhookHttpService extends BaseHttpService with Json4sSupport with IoShared {
  val sandboxPathPrefix: String
  val hookPathPrefix: String
  val exec: Executor

  var builds: List[Build] = List.empty

  def createRoutes(dirs: List[Path], builds: List[Build]) = {
    require(dirs.nonEmpty)

    this.builds = builds
    getAssets ~ pathPrefix(sandboxPathPrefix) {
      getSourcesRoute(dirs) ~ putSourcesRoute ~ postBuildRequestRoute ~ postRunRequestRoute
    } ~ pathPrefix(hookPathPrefix) {
      hookRoute
    }
  }

  private def getAssets = {
    pathEndOrSingleSlash {
      getFromResource("sandbox/index.html")
    } ~
      getFromResourceDirectory("sandbox")
  }

  private def getSourcesRoute(dirs: List[Path]) = path("sources") {
    get {
      completeTask {
        readSources(dirs).map(d => GetSourcesResponse(d.map(automap(_).to[InputDirModel])))
      }
    }
  }


  private def putSourcesRoute = path ("sources") {
    put {
      entity(as[PutSourcesRequest]) { r => {
        val dirs = r.dirs
        val sources = r.dirs.flatMap(_.sources)
        validate(dirs.forall(_.path.nonEmpty), "Dir path can't be empty") {
          validate(sources.forall(_.name.nonEmpty), "Source name can't be empty") {
            validate(sources.forall(_.content.nonEmpty), "Source content can't be empty") {
              validate(dirs.flatMap(_.vars).forall(_.name.nonEmpty), "Variable name can't be empty") {
                completeTask {
                  saveSources(r.dirs.map(automap(_).to[InputDir])).map(_ => PutSourcesResponse)
                  //todo: readSources -> build, then updates builds
                }
              }
            }
          }
        }
      }
      }
    }
  }

  private def postBuildRequestRoute = path ("sources" / "build") {
    post {
      entity(as[PostBuildRequest]) { request => {
        validate(request.content.nonEmpty, "Content can't be empty") {
          completeTask {
            exec.build(request.content, request.vars).map {
              case \/-(_) => PostBuildSuccessResponse
              case -\/(buildFailed) => PostBuildFailureResponse(buildFailed.errors.map(e => mapBuildError(e, request.content)), buildFailed.stage)
            }
          }
        }
      }
      }
    }
  }

  private def postRunRequestRoute = path ("sources" / "run") {
    post {
      entity(as[PostRunRequest]) { request => {
        completeTask {
          val done: Task[BuildFailed \/ RunResult] = (for {
            build <- EitherT.eitherT[Task, BuildFailed, Build](exec.build(request.content, request.vars))
            run <- EitherT.eitherT[Task, BuildFailed, RunResult](exec.run(request.run, List(build)).map(_.right))
          } yield run).run

          done.map {
            case \/-(RunResult(SuccessfulRun(_, result) :: Nil)) => PostRunSuccessResponse(result.toString)
            case -\/(buildFailed) => PostRunFailureResponse(buildFailed.errors.map(e => mapBuildError(e, request.content)), buildFailed.stage)
            case _ => sys.error("Sandbox: Not supposed to be here")
          }
        }
      }
      }
    }
  }

  def hookRoute: Route = ctx => {
    val path = ctx.request.uri.path.toString
    val uri = path.substring(path.indexOf(hookPathPrefix) + hookPathPrefix.length)
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

  case class BuildErrorModel(startOffset: Int, endOffset: Int, startCoordinates: ErrorCoordinates, endCoordinates: ErrorCoordinates, message: String)
  def mapBuildError(e: BuildError, content: String): BuildErrorModel = BuildErrorModel(e.startOffset, e.endOffset, errorCoordinates(content, e.startOffset), errorCoordinates(content, e.endOffset), e.message)

  case class InputSourceModel(name: String, content: String)
  case class InputDirModel(path: String, sources: List[InputSourceModel], vars: List[Variable[String]])

  case class GetSourcesResponse(dirs: List[InputDirModel])

  case class PutSourcesRequest(dirs: List[InputDirModel])
  case object PutSourcesResponse

  case class PostBuildRequest(content: String, vars: List[Variable[String]])
  case object PostBuildSuccessResponse
  case class PostBuildFailureResponse(errors: List[BuildErrorModel], stage: String)

  case class PostRunRequest(content: String, vars: List[Variable[String]], run: HttpRequest)
  case class PostRunSuccessResponse(result: String)
  case class PostRunFailureResponse(errors: List[BuildErrorModel], stage: String)
}
