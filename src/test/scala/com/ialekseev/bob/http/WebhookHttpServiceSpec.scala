package com.ialekseev.bob.http

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{StatusCodes}
import com.ialekseev.bob.analyzer.Analyzer.{ScalaCode, Webhook, Namespace, AnalysisResult}
import com.ialekseev.bob.exec.Executor.{SuccessfulRun, RunResult, Build}
import com.ialekseev.bob.http.WebhookHttpService.{HttpResponseRun, HttpResponse}
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

    "GET request has just uri AND there IS a matching build" should {
      "succeed with the build" in {
        //arrange
        val uri = "/example/1"
        val method = HttpMethod.GET
        val build = {
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some(uri), method, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "code")
        }
        val request = HttpRequest(some(uri), method, Map.empty, Map.empty, none)
        when(exec.run(request, Seq(build))).thenReturn(Task.now(RunResult(Seq(SuccessfulRun(build, "1")))))

        //act
        Get(uri) ~> createRoute(Seq(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, Seq(HttpResponseRun(Namespace("com", "create"), some("1"), none))))
        }
      }
    }

    "POST request has uri & headers AND there IS a matching build" should {
      "succeed with the build" in {
        //arrange
        val uri = "/example/1"
        val method = HttpMethod.POST
        val headers = Map("h1" -> "super", "head2" -> "cool")
        val build = {
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("/example/1"), method, headers, Map.empty, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "code")
        }
        val request = HttpRequest(some(uri), method, headers, Map.empty, none)
        when(exec.run(request, Seq(build))).thenReturn(Task.now(RunResult(Seq(SuccessfulRun(build, "1")))))

        //act
        Post(uri).withHeaders(RawHeader("h1", "super"), RawHeader("head2", "cool")) ~> createRoute(Seq(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, Seq(HttpResponseRun(Namespace("com", "create"), some("1"), none))))
        }
      }
    }

    "PUT request has uri & queryString AND there IS a matching build" should {
      "succeed with the build" in {
        //arrange
        val path = "/example/1"
        val uri = path + "?q1=super&query2=cool"
        val method = HttpMethod.PUT
        val queryString = Map("q1" -> "super", "query2" -> "cool")
        val build = {
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("/example/1"), method, Map.empty, queryString, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "code")
        }
        val request = HttpRequest(some(path), method, Map.empty, queryString, none)
        when(exec.run(request, Seq(build))).thenReturn(Task.now(RunResult(Seq(SuccessfulRun(build, "1")))))

        //act
        Put(uri) ~> createRoute(Seq(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, Seq(HttpResponseRun(Namespace("com", "create"), some("1"), none))))
        }
      }
    }

    "GET request has just uri AND there are NO matching builds" should {
      "succeed with empty Seq response" in {
        //arrange
        val uri = "/example/1"
        val method = HttpMethod.GET
        val build = {
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", Seq("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("/example/2"), method, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "code")
        }
        val request = HttpRequest(some(uri), method, Map.empty, Map.empty, none)
        when(exec.run(request, Seq(build))).thenReturn(Task.now(RunResult(Seq.empty))) //NO matching builds

        //act
        Get(uri) ~> createRoute(Seq(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, Seq.empty))
        }
      }
    }

    //todo: cover FailedRun cases
  }
}
