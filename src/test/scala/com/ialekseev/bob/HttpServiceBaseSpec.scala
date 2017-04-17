package com.ialekseev.bob

import java.util.concurrent.TimeUnit
import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.stream.scaladsl.StreamConverters
import scala.concurrent.duration.FiniteDuration
import scala.io.Source

trait HttpServiceBaseSpec extends BaseSpec with ScalatestRouteTest {
  implicit val executionContext = system.dispatcher
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(askTimeout.duration)

  def responseAsString: String = {
    Source.fromInputStream(responseAs[HttpResponse].entity.httpEntity.dataBytes.runWith(StreamConverters.asInputStream(FiniteDuration(1, TimeUnit.SECONDS)))).mkString
  }
}
