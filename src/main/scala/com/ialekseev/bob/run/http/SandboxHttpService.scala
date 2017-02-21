package com.ialekseev.bob.run.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.exec.Executor.{Build, RunResult, SuccessfulRun}
import com.ialekseev.bob.run.IoShared
import com.ialekseev.bob.run.http.SandboxHttpService._
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s._
import org.json4s.ext.EnumNameSerializer
import scalaz.concurrent.Task
import scalaz.std.option._
import scalaz.syntax.either._
import scalaz.{-\/, EitherT, \/, \/-}

trait SandboxHttpService extends BaseHttpService with Json4sSupport with IoShared {
  implicit val formats = DefaultFormats + FieldSerializer[PostBuildResponse]() + FieldSerializer[PostRunResponse]() + new EnumNameSerializer(HttpMethod) + new BodySerializer
  implicit val serialization = native.Serialization

  val sandboxExecutor: Executor

  def createRoutes(dir: String): Route = getSourcesRoute(dir) ~ getOneSourceRoute ~ putOneSourceRoute ~ postBuildRequestRoute ~ postRunRequestRoute

  private def getSourcesRoute(dir: String) = path ("sandbox" / "sources") {
    get {
      completeIO {
        for {
          list <- listFiles(dir)
          vars <- extractVarsForDir(dir)
        } yield GetSourcesResponse(list, vars)
      }
    }
  }

  private def getOneSourceRoute = path ("sandbox" / "sources" / Segment) { filePath => {
      get {
        completeIO {
          readFile(filePath).map(GetOneSourceResponse(filePath, _))
        }
      }
    }
  }

  private def putOneSourceRoute = path ("sandbox" / "sources" / Segment) { filePath => {
    put {
      entity(as[PutOneSourceRequest]) { source => {
        validate(source.content.nonEmpty, "Content can't be empty") {
          completeIO {
            updateFile(filePath, source.content).map(_ => PutOneSourceResponse(filePath))
              }
            }
          }
        }
      }
    }
  }

  private def postBuildRequestRoute = path ("sandbox" / "sources" / "compile") {
    post {
      entity(as[PostBuildRequest]) { request => {
        validate(request.content.nonEmpty, "Content can't be empty") {
          completeTask {
            sandboxExecutor.build(request.content, request.vars).map {
              case \/-(build) => PostBuildResponse(Nil)
              case -\/(buildFailed) => PostBuildResponse(buildFailed.errors.map(mapBuildError))
              }
            }
          }
        }
      }
    }
  }

  private def postRunRequestRoute = path ("sandbox" / "sources" / "run") {
    post {
      entity(as[PostRunRequest]) { request => {
          completeTask {
            val done: Task[BuildFailed \/ RunResult] = (for {
              build <- EitherT.eitherT[Task, BuildFailed, Build](sandboxExecutor.build(request.content, request.vars))
              run <- EitherT.eitherT[Task, BuildFailed, RunResult](sandboxExecutor.run(request.run, List(build)).map(_.right))
            } yield run).run

            done.map {
              case \/-(RunResult(SuccessfulRun(_, result) :: Nil)) => PostRunResponse(some(result.toString), Nil)
              case -\/(buildFailed) => PostRunResponse(none, buildFailed.errors.map(mapBuildError))
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

  case class GetSourcesResponse(list: List[String], vars: List[(String, String)])
  case class GetOneSourceResponse(filePath: String, content: String)

  case class PutOneSourceRequest(content: String)
  case class PutOneSourceResponse(updated: String)

  case class PostBuildRequest(content: String, vars: List[(String, String)])
  case class PostBuildResponse(errors: List[BuildErrorResponse]) {
    val succeed = errors.isEmpty
  }

  case class PostRunRequest(content: String, vars: List[(String, String)], run: HttpRequest)
  case class PostRunResponse(result: Option[String], errors: List[BuildErrorResponse]) {
    val succeed =  errors.isEmpty
  }
}


