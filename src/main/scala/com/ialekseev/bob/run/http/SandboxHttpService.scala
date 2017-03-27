package com.ialekseev.bob.run.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.exec.Executor.{Build, RunResult, SuccessfulRun}
import com.ialekseev.bob.run.IoShared
import com.ialekseev.bob.run.http.SandboxHttpService._
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import scalaz.concurrent.Task
import scalaz.syntax.either._
import scalaz.{-\/, EitherT, \/, \/-}

trait SandboxHttpService extends BaseHttpService with Json4sSupport with IoShared {
  val exec: Executor

  def createRoutes(dir: String): Route = getAssets ~ pathPrefix("sandbox") {
     getSourcesRoute(dir) ~ getOneSourceRoute ~ putOneSourceRoute ~ putVarsRoute ~ postBuildRequestRoute ~ postRunRequestRoute
  }

  private def getAssets = {
    pathEndOrSingleSlash {
      getFromResource("sandbox/index.html")
    } ~
    getFromResourceDirectory("sandbox")
  }

  private def getSourcesRoute(dir: String) = path ("sources") {
    get {
      completeTask {
        for {
          list <- listSourceFiles(dir)
          vars <- extractVarsForDir(dir)
        } yield GetSourcesResponse(dir, list, vars)
      }
    }
  }

  private def getOneSourceRoute = path ("sources" / Segment) { filePath => {
      get {
        completeTask {
          readFile(filePath).map(GetOneSourceResponse(filePath, _))
        }
      }
    }
  }

  private def putOneSourceRoute = path ("sources" / Segment) { filePath => {
    put {
      entity(as[PutOneSourceRequest]) { source => {
        validate(source.content.nonEmpty, "Content can't be empty") {
          completeTask {
            updateFile(filePath, source.content).map(_ => PutOneSourceResponse(filePath))
              }
            }
          }
        }
      }
    }
  }

  private def putVarsRoute = path ("sources" / "vars" / Segment) { dir => {
    put {
      entity(as[PutVarsRequest]) { r => {
        completeTask {
            for {
                  varsFile <- findVarsFile(dir)
                  _ <- updateFile(varsFile.get, pretty(render((r.vars.foldLeft(JObject())((js, v) => js ~ (v.name -> v.value))))))
                } yield PutVarsResponse(varsFile.get)
            }
          }
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

  case class GetSourcesResponse(dir: String, list: List[String], vars: List[Variable[String]])
  case class GetOneSourceResponse(filePath: String, content: String)

  case class PutOneSourceRequest(content: String)
  case class PutOneSourceResponse(updated: String)

  case class PutVarsRequest(vars: List[Variable[String]])
  case class PutVarsResponse(updated: String)

  case class PostBuildRequest(content: String, vars: List[Variable[String]])
  case object PostBuildSuccessResponse
  case class PostBuildFailureResponse(errors: List[BuildErrorResponse], stage: String)

  case class PostRunRequest(content: String, vars: List[Variable[String]], run: HttpRequest)
  case class PostRunSuccessResponse(result: String)
  case class PostRunFailureResponse(errors: List[BuildErrorResponse], stage: String)
}


