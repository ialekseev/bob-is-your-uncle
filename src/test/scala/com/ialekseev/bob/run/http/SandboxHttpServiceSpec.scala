package com.ialekseev.bob.run.http

import java.io.FileNotFoundException
import java.nio.file.{Path, Paths}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.ialekseev.bob.exec.Executor.{Build, RunResult, SuccessfulRun}
import com.ialekseev.bob.exec.analyzer.Analyzer.{AnalysisResult, Namespace, ScalaCode, Webhook}
import com.ialekseev.bob.run.boot.HttpServiceUnsafe
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.run.{ErrorCoordinates, InputDir, InputSource}
import com.ialekseev.bob.run.http.SandboxHttpService._
import org.json4s.JsonAST.{JField, JObject, JString}
import org.mockito.Mockito._
import org.json4s.native.JsonMethods._
import scalaz.concurrent.Task
import scalaz.std.option._
import scalaz.syntax.either._

class SandboxHttpServiceSpec extends SandboxHttpService with HttpServiceUnsafe with HttpServiceBaseSpec {
  val exec: Executor = mock[Executor]
  override def beforeEach(): Unit = { reset(exec); super.beforeEach()}

  private var readSourcesFunc: List[Path] => Task[List[InputDir]] = null
  private var updateSourcesFunc: List[InputDir] => Task[Unit] = null

  override def readSources(dirs: List[Path]): Task[List[InputDir]] = readSourcesFunc(dirs)
  override def saveSources(dirs: List[InputDir]): Task[Unit] = updateSourcesFunc(dirs)

  val dir = Paths.get("\\test")

  "GET sources request" when {

    "IO returns sources" should {
      "return 'OK' with the sources" in {
        //arrange
        val sourcesToReturn = List(InputDir("\\test\\", List(InputSource("file1", "content1"), InputSource("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))))
        readSourcesFunc = {
          case path if path.head.toString == "\\test" => Task.now(sourcesToReturn)
        }

        //act
        Get("/sandbox/sources") ~> createRoutes(dir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(List(InputDirModel("\\test\\", List(InputSourceModel("file1", "content1"), InputSourceModel("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))))))
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        readSourcesFunc = {
          case path if path.map(_.getFileName) == "\\test\\" :: Nil => Task.fail(new FileNotFoundException("bad!"))
        }

        //act
        Get("/sandbox/sources") ~> createRoutes(dir) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
        }
      }
    }
  }

  "PUT sources request" when {

    "IO updates sources" should {
      "return 'OK'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test\\", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")

        updateSourcesFunc = {
          case InputDir("\\test\\", List(InputSource("file1", "content1"), InputSource("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))) :: Nil => Task.now((): Unit)
        }

        //act
        Put("/sandbox/sources", put) ~> createRoutes(dir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutSourcesResponse.type] should be(PutSourcesResponse)
        }
      }
    }

    "Client sends empty dir path" should {
      "return 'BadRequest'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")

        //act
        Put("/sandbox/sources", put) ~> Route.seal(createRoutes(dir)) ~> check {

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
        Put("/sandbox/sources", put) ~> Route.seal(createRoutes(dir)) ~> check {

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
        Put("/sandbox/sources", put) ~> Route.seal(createRoutes(dir)) ~> check {

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
        Put("/sandbox/sources", put) ~> Route.seal(createRoutes(dir)) ~> check {

          //assert
          response.status should be(StatusCodes.BadRequest)
          responseAsString should be ("Variable name can't be empty")
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test\\", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")

        updateSourcesFunc = {
          case InputDir("\\test\\", List(InputSource("\\test\\file1.bob", "content1"), InputSource("\\test\\file2.bob", "content2")), List(Variable("a", "1"), Variable("b", "2"))) :: Nil => Task.now((): Unit)
        }

        //act
        Put("/sandbox/sources", put) ~> createRoutes(dir) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
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
        Post("/sandbox/sources/build", post) ~> createRoutes(dir) ~> check {

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
        Post("/sandbox/sources/build", post) ~> createRoutes(dir) ~> check {

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
        Post("/sandbox/sources/build", post) ~> Route.seal(createRoutes(dir)) ~> check {

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
        Post("/sandbox/sources/build", post) ~> createRoutes(dir) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
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
        Post("/sandbox/sources/run", post) ~> createRoutes(dir) ~> check {

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
        Post("/sandbox/sources/run", post) ~> createRoutes(dir) ~> check {

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
        Post("/sandbox/sources/run", post) ~> createRoutes(dir) ~> check {

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
        Post("/sandbox/sources/run", post) ~> createRoutes(dir) ~> check {

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
        Post("/sandbox/sources/run", post) ~> createRoutes(dir) ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
        }
      }
    }
  }
}
