package com.ialekseev.bob.http

import akka.http.scaladsl.model.{StatusCodes}
import akka.http.scaladsl.server.Directives._
import com.ialekseev.bob.IoShared
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{native, DefaultFormats}
import scalaz._

trait SandboxHttpService extends Json4sSupport with IoShared {
  implicit val formats = DefaultFormats
  implicit val serialization = native.Serialization

  def createRoutes(dir: String) = listAllFilesRoute(dir)

  private def listAllFilesRoute(dir: String) =
    get {
      pathSingleSlash {
        listSourceFiles(dir).run.unsafePerformIO() match {
          case \/-(files) => complete(files.map(_.getPath))
          case -\/(errors) => complete(StatusCodes.InternalServerError, errors.map(_.getMessage))
        }
      }
    }
}
