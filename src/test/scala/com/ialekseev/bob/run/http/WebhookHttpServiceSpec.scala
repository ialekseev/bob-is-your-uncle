package com.ialekseev.bob.run.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import com.ialekseev.bob.exec.analyzer.Analyzer.{AnalysisResult, Namespace, ScalaCode, Webhook}
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.exec.Executor.{Build, FailedRun, RunResult, SuccessfulRun}
import com.ialekseev.bob.run.boot.HttpServiceUnsafe
import com.ialekseev.bob.run.http.WebhookHttpService.{HttpResponse, HttpResponseRun}
import com.ialekseev.bob._
import org.mockito.Mockito._
import scalaz.concurrent.Task
import scalaz.std.option._

class WebhookHttpServiceSpec extends WebhookHttpService with HttpServiceUnsafe with HttpServiceBaseSpec {

  val exec = mock[Executor]
  override def beforeEach(): Unit = { reset(exec); super.beforeEach()}

  "Webhook routing" when {

    "GET request has just uri AND there IS a matching build" should {
      "succeed with the build" in {
        //arrange
        val uri = "/example/1"
        val method = HttpMethod.GET
        val build = {
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some(uri), method, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "super", List(1,2,3))
        }
        val request = HttpRequest(some(uri), method, Map.empty, Map.empty, none)
        when(exec.run(request, List(build))).thenReturn(Task.now(RunResult(List(SuccessfulRun(build, "1")))))

        //act
        Get("/hook" + uri) ~> createRoute(List(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, List(HttpResponseRun(Namespace("com", "create"), true))))
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
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("/example/1"), method, headers, Map.empty, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "super", List(1,2,3))
        }
        val request = HttpRequest(some(uri), method, headers, Map.empty, none)
        when(exec.run(request, List(build))).thenReturn(Task.now(RunResult(List(SuccessfulRun(build, "1")))))

        //act
        Post("/hook" + uri).withHeaders(RawHeader("h1", "super"), RawHeader("head2", "cool")) ~> createRoute(List(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, List(HttpResponseRun(Namespace("com", "create"), true))))
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
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("/example/1"), method, Map.empty, queryString, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "super", List(1,2,3))
        }
        val request = HttpRequest(some(path), method, Map.empty, queryString, none)
        when(exec.run(request, List(build))).thenReturn(Task.now(RunResult(List(SuccessfulRun(build, "1")))))

        //act
        Put("/hook" + uri) ~> createRoute(List(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, List(HttpResponseRun(Namespace("com", "create"), true))))
        }
      }
    }

    "GET request has just uri AND there are NO matching builds" should {
      "succeed with empty Seq response" in {
        //arrange
        val uri = "/example/1"
        val method = HttpMethod.GET
        val build = {
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("/example/2"), method, Map.empty, Map.empty, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "super", List(1,2,3))
        }
        val request = HttpRequest(some(uri), method, Map.empty, Map.empty, none)
        when(exec.run(request, List(build))).thenReturn(Task.now(RunResult(List.empty))) //NO matching builds

        //act
        Get("/hook" + uri) ~> createRoute(List(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, List.empty))
        }
      }
    }

    "PUT request has uri & queryString AND there IS a matching build (but run failed)" should {
      "return failure for that run" in {
        //arrange
        val path = "/example/1"
        val uri = path + "?q1=super&query2=cool"
        val method = HttpMethod.PUT
        val queryString = Map("q1" -> "super", "query2" -> "cool")
        val build = {
          val analysisResult = AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("/example/1"), method, Map.empty, queryString, none[Body])), ScalaCode("do()"))
          Build(analysisResult, "super", List(1,2,3))
        }
        val request = HttpRequest(some(path), method, Map.empty, queryString, none)
        when(exec.run(request, List(build))).thenReturn(Task.now(RunResult(List(FailedRun(build)))))

        //act
        Put("/hook" + uri) ~> createRoute(List(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, List(HttpResponseRun(Namespace("com", "create"), false))))
        }
      }
    }
  }
}
