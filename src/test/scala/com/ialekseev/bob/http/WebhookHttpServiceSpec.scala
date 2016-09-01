package com.ialekseev.bob.http

import akka.http.scaladsl.model.StatusCodes
import com.ialekseev.bob.analyzer.Analyzer.{ScalaCode, Webhook, Namespace, AnalysisResult}
import com.ialekseev.bob.exec.Executor.{Run, Build}
import com.ialekseev.bob.{Body, HttpMethod, HttpRequest, BaseSpec}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.mockito.Mockito._
import com.ialekseev.bob.exec.Executor
import scalaz.std.option._
import scalaz.concurrent.Task

class WebhookHttpServiceSpec extends WebhookHttpService with BaseSpec with ScalatestRouteTest {

  val exec = mock[Executor]
  override def beforeEach(): Unit = { reset(exec); super.beforeEach()}

  "Routing" when {

    "GET request has just uri there IS a matching build" should {
      "run" in {
        //arrange
        val request = HttpRequest("/example/1", HttpMethod.GET, Map.empty, Map.empty, none)
        val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest("/example/1", HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
        val build = Build(analysisResult, "code")
        when(exec.run(request, Seq(build))).thenReturn(Task.now(Seq(Run(build, "1"))))

        //act
        Get("/example/1") ~> createRoute(Seq(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[Seq[Namespace]] should be (Seq(Namespace("com", "create")))
        }
      }
    }
  }
}
