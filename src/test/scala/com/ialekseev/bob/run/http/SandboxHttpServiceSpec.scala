package com.ialekseev.bob.run.http

import java.io.{FileNotFoundException}
import akka.http.scaladsl.model.{StatusCodes}
import akka.http.scaladsl.server.{Route}
import com.ialekseev.bob.exec.Executor.{SuccessfulRun, RunResult, Build}
import com.ialekseev.bob.exec.analyzer.Analyzer.{ScalaCode, Webhook, Namespace, AnalysisResult}
import com.ialekseev.bob.run.boot.HttpServiceUnsafe
import com.ialekseev.bob._
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.run.http.SandboxHttpService._
import org.json4s.JsonAST.{JString, JField, JObject}
import org.mockito.Mockito._
import org.json4s.native.JsonMethods._
import scalaz.concurrent.Task
import scalaz.std.option._
import scalaz.syntax.either._

class SandboxHttpServiceSpec extends SandboxHttpService with HttpServiceUnsafe with HttpServiceBaseSpec {
  val sandboxExecutor: Executor = mock[Executor]
  override def beforeEach(): Unit = { reset(sandboxExecutor); super.beforeEach()}

  private var listFilesFunc: String => IoTry[List[String]] = null
  private var extractVarsForDirFunc: String => IoTry[List[(String, String)]] = null
  private var readFileFunc: String => IoTry[String] = null
  private var updateFileFunc: (String, String) => IoTry[Unit] = null

  override def listFiles(dir: String): IoTry[List[String]] = listFilesFunc(dir)
  override def extractVarsForDir(dir: String): IoTry[List[(String, String)]] = extractVarsForDirFunc(dir)
  override def readFile(filePath: String): IoTry[String] = readFileFunc(filePath)
  override def updateFile(filePath: String, content: String): IoTry[Unit] = updateFileFunc(filePath, content)

  "GET list of all files request" when {

    "IO returns list of files" should {
      "return 'OK' with the files" in {

        //arrange
        listFilesFunc = {
          case "\\test\\" => IoTry.success(List("\\test\\file1.bob", "\\test\\file2.bob"))
        }

        extractVarsForDirFunc = {
          case "\\test\\" => IoTry.success(List("a" -> "1", "b" -> "2"))
        }

        //act
        Get("/sandbox/sources") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(List("\\test\\file1.bob", "\\test\\file2.bob"), List("a" -> "1", "b" -> "2")))
        }
      }
    }

    "IO fails to list files" should {
      "return 'InternalServerError'" in {
        //arrange
        listFilesFunc = {
          case "\\test\\" => IoTry.failure(new FileNotFoundException("bad list!"))
        }

        //act
        Get("/sandbox/sources") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
        }
      }
    }

    "IO fails to provide vars" should {
      "return 'InternalServerError'" in {
        //arrange
        listFilesFunc = {
          case "\\test\\" => IoTry.success(List("\\test\\file1.bob", "\\test\\file2.bob"))
        }

        extractVarsForDirFunc = {
          case "\\test\\" => IoTry.failure(new FileNotFoundException("bad var!"))
        }

        //act
        Get("/sandbox/sources") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
        }
      }
    }
  }

  "GET one source request" when {

    "IO returns file" should {
      "return 'OK' with the file" in {
        //arrange
        readFileFunc = {
          case "\\test\\file1.bob" => IoTry.success("content")
        }

        //act
        Get("/sandbox/sources/%5Ctest%5Cfile1.bob") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetOneSourceResponse] should be(GetOneSourceResponse("\\test\\file1.bob", "content"))
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        readFileFunc = {
          case "\\test\\file1.bob" => IoTry.failure(new FileNotFoundException("bad!"))
        }

        //act
        Get("/sandbox/sources/%5Ctest%5Cfile1.bob") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
        }
      }
    }
  }

  "PUT one source request" when {

    "IO updates file" should {
      "return 'OK' with the file" in {
        //arrange
        val put = parse("""{"content":"content1"}""")
        updateFileFunc = {
          case ("\\test\\file1.bob", "content1") => IoTry.success((): Unit)
        }

        //act
        Put("/sandbox/sources/%5Ctest%5Cfile1.bob", put) ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutOneSourceResponse] should be(PutOneSourceResponse("\\test\\file1.bob"))
        }
      }
    }

    "Client sends empty content" should {
      "return 'BadRequest'" in {
        //arrange
        val put = parse("""{"content":""}""")

        //act
        Put("/sandbox/sources/%5Ctest%5Cfile1.bob", put) ~> Route.seal(createRoutes("\\test\\")) ~> check {

          //assert
          response.status should be(StatusCodes.BadRequest)
          responseAsString should be ("Content can't be empty")
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        val put = parse("""{"content":"content1"}""")
        updateFileFunc = {
          case ("\\test\\file1.bob", "content1") => IoTry.failure(new FileNotFoundException("bad!"))
        }

        //act
        Put("/sandbox/sources/%5Ctest%5Cfile1.bob", put) ~> createRoutes("\\test\\") ~> check {

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
        val resultToBeReturned = Build(AnalysisResult(Namespace("com", "create"), "cool", List("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(sandboxExecutor.build("content1", List("a" -> "1", "b" -> "2"))).thenReturn(Task.now(resultToBeReturned.right))

        val post = parse("""{"content":"content1", "vars": [{"a": "1"}, {"b": "2"}]}""")

        //act
        Post("/sandbox/sources/compile", post) ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildResponse] should be(PostBuildResponse(Nil))
        }
      }
    }

    "Executor returns a failed build" should {
      "return 'OK' with failed build details" in {
        //arrange
        val resultToBeReturned = SyntaxAnalysisFailed(List(SyntaxError(1, 2, 5, "Bad!")))
        when(sandboxExecutor.build("content1", List("a" -> "1", "b" -> "2"))).thenReturn(Task.now(resultToBeReturned.left))

        val post = parse("""{"content":"content1", "vars": [{"a": "1"}, {"b": "2"}]}""")

        //act
        Post("/sandbox/sources/compile", post) ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildResponse] should be(PostBuildResponse(List(BuildErrorResponse(1, 2, "Bad!"))))
        }
      }
    }

    "Client sends empty content" should {
      "return 'BadRequest'" in {
        //arrange
        val post = parse("""{"content":"", "vars": [{"a": "1"}, {"b": "2"}]}""")

        //act
        Post("/sandbox/sources/compile", post) ~> Route.seal(createRoutes("\\test\\")) ~> check {

          //assert
          response.status should be(StatusCodes.BadRequest)
          responseAsString should be ("Content can't be empty")
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"a": "1"}, {"b": "2"}]}""")
        when(sandboxExecutor.build("content1", List("a" -> "1", "b" -> "2"))).thenReturn(Task.fail(new FileNotFoundException("bad!")))

        //act
        Post("/sandbox/sources/compile", post) ~> createRoutes("\\test\\") ~> check {

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
        val post = parse("""{"content":"content1", "vars": [{"a": "1"}, {"b": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"text": "body!"} } }""")
        val buildToBeReturned = Build(AnalysisResult(Namespace("com", "create"), "cool", List("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(sandboxExecutor.build("content1", List("a" -> "1", "b" -> "2"))).thenReturn(Task.now(buildToBeReturned.right))
        when(sandboxExecutor.run(HttpRequest(some("/hello/"), HttpMethod.GET, Map("h1" -> "1"), Map("q2" -> "2"), some(StringLiteralBody("body!"))), List(buildToBeReturned))).thenReturn(Task.now(RunResult(List(SuccessfulRun(buildToBeReturned, "done!")))))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunResponse] should be(PostRunResponse(some("done!"), Nil))
        }
      }
    }

    "Executor returns a successful Run (for incoming request having dic body)" should {
      "return 'OK' with run details" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"a": "1"}, {"b": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"dic": {"par1": "val1", "par2": "val2"} } } }""")
        val buildToBeReturned = Build(AnalysisResult(Namespace("com", "create"), "cool", List("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(sandboxExecutor.build("content1", List("a" -> "1", "b" -> "2"))).thenReturn(Task.now(buildToBeReturned.right))
        when(sandboxExecutor.run(HttpRequest(some("/hello/"), HttpMethod.GET, Map("h1" -> "1"), Map("q2" -> "2"), some(DictionaryBody(Map("par1"->"val1", "par2"->"val2")))), List(buildToBeReturned))).thenReturn(Task.now(RunResult(List(SuccessfulRun(buildToBeReturned, "done!")))))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunResponse] should be(PostRunResponse(some("done!"), Nil))
        }
      }
    }

    "Executor returns a successful Run (for incoming request having json body)" should {
      "return 'OK' with run details" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"a": "1"}, {"b": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"json": "{\"f1\": \"val1\", \"f2\": \"val2\"}" } } }""")
        val buildToBeReturned = Build(AnalysisResult(Namespace("com", "create"), "cool", List("a" -> "1", "b" -> "2"), Webhook(HttpRequest(some("example/"), HttpMethod.GET, Map.empty, Map.empty, none[Body])), ScalaCode("do()")), "super", List(1,2,3))
        when(sandboxExecutor.build("content1", List("a" -> "1", "b" -> "2"))).thenReturn(Task.now(buildToBeReturned.right))
        when(sandboxExecutor.run(HttpRequest(some("/hello/"), HttpMethod.GET, Map("h1" -> "1"), Map("q2" -> "2"), some(JsonBody(JObject(JField("f1", JString("val1")) :: JField("f2", JString("val2")) :: Nil)))), List(buildToBeReturned))).thenReturn(Task.now(RunResult(List(SuccessfulRun(buildToBeReturned, "done!")))))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunResponse] should be(PostRunResponse(some("done!"), Nil))
        }
      }
    }

    "Executor returns a failed Build" should {
      "return 'OK' with failed build details" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"a": "1"}, {"b": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"json": "{\"f1\": \"val1\", \"f2\": \"val2\"}" } } }""")

        val resultToBeReturned = SyntaxAnalysisFailed(List(SyntaxError(1, 2, 5, "Bad!")))
        when(sandboxExecutor.build("content1", List("a" -> "1", "b" -> "2"))).thenReturn(Task.now(resultToBeReturned.left))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunResponse] should be(PostRunResponse(none, List(BuildErrorResponse(1, 2, "Bad!"))))
        }
      }
    }

    "Build's IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        val post = parse("""{"content":"content1", "vars": [{"a": "1"}, {"b": "2"}], "run": {"uri": "/hello/", "method": "GET", "headers": {"h1" : "1"}, "queryString": {"q2" : "2"}, "body": {"dic": {"par1": "val1", "par2": "val2"} } } }""")
        when(sandboxExecutor.build("content1", List("a" -> "1", "b" -> "2"))).thenReturn(Task.fail(new FileNotFoundException("bad!")))

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
        }
      }
    }
  }
}
