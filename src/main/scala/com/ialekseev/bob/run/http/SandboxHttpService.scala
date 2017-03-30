package com.ialekseev.bob.run.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.exec.Executor.{Build, RunResult, SuccessfulRun}
import com.ialekseev.bob.run.{InputDir, IoShared}
import com.ialekseev.bob.run.http.SandboxHttpService._
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import scalaz.concurrent.Task
import scalaz.syntax.either._
import scalaz.{-\/, EitherT, \/, \/-}
import com.bfil.automapper._

trait SandboxHttpService extends BaseHttpService with Json4sSupport with IoShared {
  val exec: Executor

  def createRoutes(dir: String): Route = getAssets ~ pathPrefix("sandbox") {
     getSourcesRoute(dir) ~ putSourcesRoute ~ postBuildRequestRoute ~ postRunRequestRoute
  }

  private def getAssets = {
    pathEndOrSingleSlash {
      getFromResource("sandbox/index.html")
    } ~
    getFromResourceDirectory("sandbox")
  }

  //todo: load & update all sources as a whole. This will make client-side experience better

  private def getSourcesRoute(dir: String) = path("sources") {
    get {
      completeTask {
        readSources(List(dir)).map(d => GetSourcesResponse(automap(d).to[List[InputDirModel]]))
      }
    }
  }


  private def putSourcesRoute = path ("sources") {
    put {
      entity(as[PutSourcesRequest]) { r => {
          //todo: implement with validation etc
          ???
        }
      }
    }
  }

  private def postBuildRequestRoute = path ("sources" / "compile") {
    post {
      entity(as[PostBuildRequest]) { request => {
        validate(request.content.nonEmpty, "Content can't be empty") {
          completeTask {
            exec.build(request.content, request.vars).map {
              case \/-(build) => PostBuildSuccessResponse
              case -\/(buildFailed) => PostBuildFailureResponse(buildFailed.errors.map(mapBuildError(_)), buildFailed.stage)
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
              case -\/(buildFailed) => PostRunFailureResponse(buildFailed.errors.map(mapBuildError), buildFailed.stage)
              case _ => sys.error("Sandbox: Not supposed to be here")
            }
          }
        }
      }
    }
  }
}

object SandboxHttpService {
  case class BuildErrorResponse(startOffset: Int, endOffset: Int, message: String)
  def mapBuildError(e: BuildError): BuildErrorResponse = BuildErrorResponse(e.startOffset, e.endOffset, e.message)

  case class InputSourceModel(path: String, content: String)
  case class InputDirModel(path: String, sources: List[InputSourceModel], vars: List[Variable[String]])

  case class GetSourcesResponse(dirs: List[InputDirModel])
  case class GetOneSourceResponse(filePath: String, content: String)

  case class PutSourcesRequest(dirs: List[InputDirModel])
  case class PutSourcesResponse(updated: String)

  case class PostBuildRequest(content: String, vars: List[Variable[String]])
  case object PostBuildSuccessResponse
  case class PostBuildFailureResponse(errors: List[BuildErrorResponse], stage: String)

  case class PostRunRequest(content: String, vars: List[Variable[String]], run: HttpRequest)
  case class PostRunSuccessResponse(result: String)
  case class PostRunFailureResponse(errors: List[BuildErrorResponse], stage: String)
}


