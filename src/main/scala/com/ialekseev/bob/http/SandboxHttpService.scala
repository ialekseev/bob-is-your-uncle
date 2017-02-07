package com.ialekseev.bob.http

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.{StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.StandardRoute
import com.ialekseev.bob.IoShared
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.http.SandboxHttpService.{PutOneSourceResponse, GetOneSourceResponse, GetSourcesResponse, PutOneSourceRequest}
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{native, DefaultFormats}
import scalaz._

trait SandboxHttpService extends Json4sSupport with IoShared {
  implicit val formats = DefaultFormats
  implicit val serialization = native.Serialization

  val sandboxExecutor: Executor

  def createRoutes(dir: String) = getSourcesRoute(dir) ~ getOneSourceRoute ~ putOneSourceRoute

  private def getSourcesRoute(dir: String) = path ("sandbox" / "sources") {
    get {
      unsafeComplete {
        for {
          list <- listFiles(dir)
          vars <- extractVarsForDir(dir)
        } yield GetSourcesResponse(list, vars.toMap)
      }
    }
  }

  private def getOneSourceRoute = path ("sandbox" / "sources" / Segment) { filePath => {
      get {
        unsafeComplete {
          readFile(filePath).map(GetOneSourceResponse(filePath, _))
        }
      }
    }
  }

  //todo: validate that content is not empty
  private def putOneSourceRoute = path ("sandbox" / "sources" / Segment) { filePath => {
    put {
      entity(as[PutOneSourceRequest]) { source => {
        unsafeComplete {
            updateFile(filePath, source.content).map(_ => PutOneSourceResponse(filePath))
            }
          }
        }
      }
    }
  }

  private def unsafeComplete[T : ToResponseMarshaller](ioTry: IoTry[T]): StandardRoute = {
    ioTry.run.unsafePerformIO() match {
      case \/-(res) => complete(res)
      case -\/(errors) => complete(StatusCodes.InternalServerError, errors.map(_.getMessage))
    }
  }
}

object SandboxHttpService {
  case class GetSourcesResponse(list: List[String], vars: Map[String, String])
  case class GetOneSourceResponse(filePath: String, content: String)

  case class PutOneSourceRequest(content: String)
  case class PutOneSourceResponse(updated: String)
}


