package com.ialekseev.bob.http

import akka.http.scaladsl.server.Directives._
import com.ialekseev.bob.InputSource
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.{native, DefaultFormats}

trait SandboxHttpService extends Json4sSupport {
  implicit val formats = DefaultFormats
  implicit val serialization = native.Serialization

  def createRoutes(sources: List[InputSource]) = getRoute(sources.map(_.path))

  private def getRoute(files: List[String]) =
    get {
      pathSingleSlash {
        complete(files)
      }
    }
}
