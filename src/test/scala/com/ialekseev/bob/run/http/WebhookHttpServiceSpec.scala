package com.ialekseev.bob.run.http

import java.io.FileNotFoundException
import java.nio.file.{Path, Paths}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import com.ialekseev.bob.exec.analyzer.Analyzer.{AnalysisResult, Namespace, ScalaCode, Webhook}
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.exec.Executor.{Build, FailedRun, RunResult, SuccessfulRun}
import com.ialekseev.bob.run.http.WebhookHttpService.{HttpResponse, HttpResponseRun}
import com.ialekseev.bob._
import com.ialekseev.bob.run.{BuildStateActor, ErrorCoordinates, InputDir, InputSource, SourceStateActor}
import WebhookHttpService._
import akka.actor.{ActorRef, Props}
import akka.testkit.TestActorRef
import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.native.JsonMethods.parse
import org.mockito.Mockito._
import scalaz.concurrent.Task
import scalaz.std.option._
import scalaz.syntax.either._

class WebhookHttpServiceSpec extends WebhookHttpService with HttpServiceBaseSpec {
  val sandboxPathPrefix = "sandbox"
  val hookPathPrefix = "hook"
  var sourceStateActor: ActorRef = null
  var buildStateActor: ActorRef = system.actorOf(Props[BuildStateActor])
  val exec = mock[Executor]

  override def beforeEach(): Unit = { reset(exec); super.beforeEach()}

  val dirs = List(Paths.get("\\test"))
  val builds: List[Build] = List.empty

  "GET sources request" when {

    "IO returns sources" should {
      "return 'OK' with the sources" in {
        //arrange
        val sourcesToReturn = List(InputDir("\\test", List(InputSource("file1", "content1"), InputSource("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))))
        sourceStateActor = TestActorRef(new SourceStateActor {
          override def readSources(dirs: List[Path]): Task[List[InputDir]] = dirs match {
            case d if d.head.toString == "\\test" => Task.now(sourcesToReturn)
          }
        })

        //act
        Get("/sandbox/sources") ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(List(InputDirModel("\\test", List(InputSourceModel("file1", "content1"), InputSourceModel("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))))))
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        sourceStateActor = TestActorRef(new SourceStateActor {
          override def readSources(dirs: List[Path]): Task[List[InputDir]] = dirs match {
            case d if d.head.toString == "\\test" => Task.fail(new FileNotFoundException("bad!"))
          }
        })

        //act
        Get("/sandbox/sources") ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
          responseAsString should be ("bad!")
        }
      }
    }
  }

  "PUT sources request" when {

    "IO updates sources & build update has NOT been requested" should {
      "return 'OK'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }], "updateBuilds": false  }""")
        sourceStateActor = TestActorRef(new SourceStateActor {
          override def saveSources(inputDirs: List[InputDir]): Task[Unit] = inputDirs match {
            case InputDir("\\test", List(InputSource("file1", "content1"), InputSource("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))) :: Nil => Task.now((): Unit)
          }
        })

        //act
        Put("/sandbox/sources", put) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutSourcesResponse] should be(PutSourcesResponse(List.empty))
        }
      }
    }

    "IO updates sources & build update HAS been requested" should {
      "save only successful builds & return 'OK'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }], "updateBuilds": true  }""")
        sourceStateActor = TestActorRef(new SourceStateActor {
          override def saveSources(inputDirs: List[InputDir]): Task[Unit] = inputDirs match {
            case InputDir("\\test", List(InputSource("file1", "content1"), InputSource("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))) :: Nil => Task.now((): Unit)
          }
        })

        val successfulBuild = Build(AnalysisResult(Namespace("com", "super"), "great", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        val failedBuild = CompilationFailed(List(CompilationError(1, 1, 1, "bad!")))
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(failedBuild.left))
        when(exec.build("content2", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(successfulBuild.right))

        //act
        Put("/sandbox/sources", put) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutSourcesResponse] should be(PutSourcesResponse(List(BuildModel("\\test", "file2", "com#super"))))
        }
      }
    }

    "IO updates sources & build update HAS been requested (but one of the build exec tasks failed)" should {
      "return 'InternalServerError'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }], "updateBuilds": true  }""")
        sourceStateActor = TestActorRef(new SourceStateActor {
          override def saveSources(inputDirs: List[InputDir]): Task[Unit] = inputDirs match {
            case InputDir("\\test", List(InputSource("file1", "content1"), InputSource("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))) :: Nil => Task.now((): Unit)
          }
        })

        val successfulBuild = Build(AnalysisResult(Namespace("com", "super"), "great", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(successfulBuild.right))
        when(exec.build("content2", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.fail(new FileNotFoundException("bad!")))

        //act
        Put("/sandbox/sources", put) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
          responseAsString should be ("bad!")
        }
      }
    }

    "Client sends empty dir path" should {
      "return 'BadRequest'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")

        //act
        Put("/sandbox/sources", put) ~> Route.seal(createRoutes(dirs, builds)) ~> check {

          //assert
          response.status should be(StatusCodes.BadRequest)
          responseAsString should be ("Dir path can't be empty")
        }
      }
    }

    "Client sends empty source name" should {
      "return 'BadRequest'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test\\", "sources": [{"name": "file1", "content": "content1"}, {"name": "", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")

        //act
        Put("/sandbox/sources", put) ~> Route.seal(createRoutes(dirs, builds)) ~> check {

          //assert
          response.status should be(StatusCodes.BadRequest)
          responseAsString should be ("Source name can't be empty")
        }
      }
    }

    "Client sends empty source content" should {
      "return 'BadRequest'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test\\", "sources": [{"name": "file1", "content": ""}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")

        //act
        Put("/sandbox/sources", put) ~> Route.seal(createRoutes(dirs, builds)) ~> check {

          //assert
          response.status should be(StatusCodes.BadRequest)
          responseAsString should be ("Source content can't be empty")
        }
      }
    }

    "Client sends a variable with empty name" should {
      "return 'BadRequest'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test\\", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "", "value": "2"}] }]  }""")

        //act
        Put("/sandbox/sources", put) ~> Route.seal(createRoutes(dirs, builds)) ~> check {

          //assert
          response.status should be(StatusCodes.BadRequest)
          responseAsString should be ("Variable name can't be empty")
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")

        sourceStateActor = TestActorRef(new SourceStateActor {
          override def saveSources(inputDirs: List[InputDir]): Task[Unit] = inputDirs match {
            case InputDir("\\test", List(InputSource("file1", "content1"), InputSource("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))) :: Nil => Task.fail(new FileNotFoundException("bad!"))
          }
        })

        //act
        Put("/sandbox/sources", put) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
          responseAsString should be ("bad!")
        }
      }
    }
  }

  "POST build request" when {

    "Executor returns a successful build" should {
      "return 'OK' with build details" in {
        //arrange
        val resultToBeReturned = Build(AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(resultToBeReturned.right))

        val post = parse("""{"content":"content1", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/build", post) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildSuccessResponse.type] shouldBe PostBuildSuccessResponse
        }
      }
    }

    "Executor returns a failed build" should {
      "return 'OK' with failed build details" in {
        //arrange
        val resultToBeReturned = SyntaxAnalysisFailed(List(SyntaxError(1, 2, 5, "Bad!")))
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(resultToBeReturned.left))

        val post = parse("""{"content":"content1", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/build", post) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildFailureResponse] should be(PostBuildFailureResponse(List(BuildErrorModel(1, 2, ErrorCoordinates(1, 2), ErrorCoordinates(1, 3), "Bad!")), "syntax"))
        }
      }
    }

    "Client sends empty content" should {
      "return 'BadRequest'" in {
        //arrange
        val post = parse("""{"content":"", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/build", post) ~> Route.seal(createRoutes(dirs, builds)) ~> check {

          //assert
          response.status should be(StatusCodes.BadRequest)
          responseAsString should be ("Content can't be empty")
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.fail(new FileNotFoundException("bad!")))

        //act
        Post("/sandbox/sources/build", post) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
          responseAsString should be ("bad!")
        }
      }
    }
  }

  "POST run request" when {

    "Executor returns a successful Run (for incoming request having text body)" should {
      "return 'OK' with run details" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"text": "body!"} } }""")
        val buildToBeReturned = Build(AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(buildToBeReturned.right))
        when(exec.run(HttpRequest(some("/hello/"), HttpMethod.GET, Map("h1" -> "1"), Map("q2" -> "2"), some(StringLiteralBody("body!"))), List(buildToBeReturned))).thenReturn(Task.now(RunResult(List(SuccessfulRun(buildToBeReturned, "done!")))))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunSuccessResponse] shouldBe PostRunSuccessResponse("done!")
        }
      }
    }

    "Executor returns a successful Run (for incoming request having dic body)" should {
      "return 'OK' with run details" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"dic": {"par1": "val1", "par2": "val2"} } } }""")
        val buildToBeReturned = Build(AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(buildToBeReturned.right))
        when(exec.run(HttpRequest(some("/hello/"), HttpMethod.GET, Map("h1" -> "1"), Map("q2" -> "2"), some(DictionaryBody(Map("par1"->"val1", "par2"->"val2")))), List(buildToBeReturned))).thenReturn(Task.now(RunResult(List(SuccessfulRun(buildToBeReturned, "done!")))))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunSuccessResponse] shouldBe PostRunSuccessResponse("done!")
        }
      }
    }

    "Executor returns a successful Run (for incoming request having json body)" should {
      "return 'OK' with run details" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"json": "{\"f1\": \"val1\", \"f2\": \"val2\"}" } } }""")
        val buildToBeReturned = Build(AnalysisResult(Namespace("com", "create"), "cool", List(Variable("a", "1"), Variable("b", "2")), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(buildToBeReturned.right))
        when(exec.run(HttpRequest(some("/hello/"), HttpMethod.GET, Map("h1" -> "1"), Map("q2" -> "2"), some(JsonBody(JObject(JField("f1", JString("val1")) :: JField("f2", JString("val2")) :: Nil)))), List(buildToBeReturned))).thenReturn(Task.now(RunResult(List(SuccessfulRun(buildToBeReturned, "done!")))))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunSuccessResponse] shouldBe PostRunSuccessResponse("done!")
        }
      }
    }

    "Executor returns a failed Build" should {
      "return 'OK' with failed build details" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"json": "{\"f1\": \"val1\", \"f2\": \"val2\"}" } } }""")

        val resultToBeReturned = SyntaxAnalysisFailed(List(SyntaxError(1, 2, 5, "Bad!")))
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.now(resultToBeReturned.left))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunFailureResponse] shouldBe PostRunFailureResponse(List(BuildErrorModel(1, 2, ErrorCoordinates(1, 2), ErrorCoordinates(1, 3), "Bad!")), "syntax")
        }
      }
    }

    "Build's IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"dic": {"par1": "val1", "par2": "val2"} } } }""")
        when(exec.build("content1", List(Variable("a", "1"), Variable("b", "2")))).thenReturn(Task.fail(new FileNotFoundException("bad!")))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes(dirs, builds) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
          responseAsString should be ("bad!")
        }
      }
    }
  }


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
        Get("/hook" + uri) ~> createRoutes(dirs, List(build)) ~> check {

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
        Post("/hook" + uri).withHeaders(RawHeader("h1", "super"), RawHeader("head2", "cool")) ~> createRoutes(dirs, List(build)) ~> check {

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
        Put("/hook" + uri) ~> createRoutes(dirs, List(build)) ~> check {

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
        Get("/hook" + uri) ~> createRoutes(dirs, List(build)) ~> check {

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
        Put("/hook" + uri) ~> createRoutes(dirs, List(build)) ~> check {

          //assert
          response.status should be (StatusCodes.OK)
          responseAs[HttpResponse] should be (HttpResponse(request, List(HttpResponseRun(Namespace("com", "create"), false))))
        }
      }
    }
  }
}
