package com.ialekseev.bob.http

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.{StatusCodes}
import akka.http.scaladsl.server.Directives._
import com.ialekseev.bob.IoShared
import com.ialekseev.bob.exec.Executor
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{native, DefaultFormats}
import scalaz._

trait SandboxHttpService extends Json4sSupport with IoShared {
  implicit val formats = DefaultFormats
  implicit val serialization = native.Serialization

  val sandboxExecutor: Executor

  def createRoutes(dir: String) = sourcesRoute(dir) ~ oneSourceRoute

  private def sourcesRoute(dir: String) = path ("sandbox" / "sources") {
    get {
      unsafeComplete(listSourceFiles(dir))
    }
  }

  private def oneSourceRoute = path ("sandbox" / "sources" / Segment) { filePath => {
      get {
        unsafeComplete(readSource(filePath))
      }
    }
  }

  private def unsafeComplete[T : ToResponseMarshaller](ioTry: IoTry[T]) = {
    ioTry.run.unsafePerformIO() match {
      case \/-(res) => complete(res)
      case -\/(errors) => complete(StatusCodes.InternalServerError, errors.map(_.getMessage))
    }
  }
}


