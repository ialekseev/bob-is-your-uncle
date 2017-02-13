package com.ialekseev.bob.run.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route}
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.run.IoShared
import com.ialekseev.bob.run.http.SandboxHttpService._
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{native, DefaultFormats}
import scalaz.{\/-, -\/}
import scalaz.std.option._

trait SandboxHttpService extends BaseHttpService with Json4sSupport with IoShared {
  implicit val formats = DefaultFormats
  implicit val serialization = native.Serialization

  val sandboxExecutor: Executor

  def createRoutes(dir: String): Route = getSourcesRoute(dir) ~ getOneSourceRoute ~ putOneSourceRoute ~ postBuildRequestRoute

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
          completeIO {
            sandboxExecutor.build(request.content, request.vars).map {
              case \/-(build) => PostBuildResponse(true, some(build.codeFileName), Nil)
              case -\/(buildFailed) => PostBuildResponse(false, none, buildFailed.errors.map(e => PostBuildResponseError(e.startOffset, e.endOffset, e.message)))
              }
            }
          }
        }
      }
    }
  }

  //todo: postRunRequestRoute
}

object SandboxHttpService {
  case class GetSourcesResponse(list: List[String], vars: List[(String, String)])
  case class GetOneSourceResponse(filePath: String, content: String)

  case class PutOneSourceRequest(content: String)
  case class PutOneSourceResponse(updated: String)

  case class PostBuildRequest(content: String, vars: List[(String, String)])
  case class PostBuildResponse(succeed: Boolean, codeFileName: Option[String], errors: List[PostBuildResponseError])
  case class PostBuildResponseError(startOffset: Int, endOffset: Int, message: String)
}


