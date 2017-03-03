package com.ialekseev.bob.run.http

import java.net.URLEncoder
import akka.actor.Props
import akka.http.scaladsl.model.StatusCodes
import com.ialekseev.bob._
import com.ialekseev.bob.exec.{EvaluatorActor, CompilerActor, Executor}
import com.ialekseev.bob.exec.analyzer.DefaultAnalyzer
import com.ialekseev.bob.run.boot.HttpServiceUnsafe
import com.ialekseev.bob.run.http.SandboxHttpService._
import org.json4s.native.JsonMethods._
import scalaz.std.option._

class SandboxHttpServiceIntegrationSpec extends SandboxHttpService with HttpServiceUnsafe with HttpServiceBaseSpec {
  val sandboxExecutor: Executor = new Executor {
    val analyzer = DefaultAnalyzer
    val compilerActor = system.actorOf(Props[CompilerActor])
    val evaluatorActor = system.actorOf(Props[EvaluatorActor])
  }
  val tempDir = System.getProperty("java.io.tmpdir")
  val tempDirEncoded = URLEncoder.encode(tempDir, "UTF-8")

  "PUT one source request" when {

    "IO updates file" should {
      "return 'OK' with the file" in {
        //arrange

        val put = parse("""{"content":"content1"}""")

        //act
        Put(s"/sandbox/sources/${tempDirEncoded}file1.bob", put) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutOneSourceResponse] should be(PutOneSourceResponse(tempDir + "file1.bob"))
        }
      }
    }
  }

  "GET one source request" when {

    "IO returns file" should {
      "return 'OK' with the file" in {
        //arrange
        Put(s"/sandbox/sources/${tempDirEncoded}file1.bob", parse("""{"content":"content1"}""")) ~> createRoutes(tempDir) ~> check { response.status should be(StatusCodes.OK) }
        Put(s"/sandbox/sources/${tempDirEncoded}file2.bob", parse("""{"content":"content2"}""")) ~> createRoutes(tempDir) ~> check { response.status should be(StatusCodes.OK) }
        Put(s"/sandbox/sources/${tempDirEncoded}file3.bob", parse("""{"content":"content3"}""")) ~> createRoutes(tempDir) ~> check { response.status should be(StatusCodes.OK) }

        //act
        Get(s"/sandbox/sources/${tempDirEncoded}file2.bob") ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetOneSourceResponse] should be(GetOneSourceResponse(tempDir + "file2.bob", "content2"))
        }
      }
    }
  }

  "GET list of all files request" when {

    "IO returns list of files" should {
      "return 'OK' with the files" in {
        //arrange
        Put(s"/sandbox/sources/${tempDirEncoded}_vars.json", parse("""{"content": "{\"a\": \"1\", \"b\": \"2\"}" }""")) ~> createRoutes(tempDir) ~> check { response.status should be(StatusCodes.OK) }
        Put(s"/sandbox/sources/${tempDirEncoded}file1.bob", parse("""{"content":"content1"}""")) ~> createRoutes(tempDir) ~> check { response.status should be(StatusCodes.OK) }
        Put(s"/sandbox/sources/${tempDirEncoded}file2.bob", parse("""{"content":"content2"}""")) ~> createRoutes(tempDir) ~> check { response.status should be(StatusCodes.OK) }
        Put(s"/sandbox/sources/${tempDirEncoded}file3.bob", parse("""{"content":"content3"}""")) ~> createRoutes(tempDir) ~> check { response.status should be(StatusCodes.OK) }

        //act
        Get("/sandbox/sources") ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(List(tempDir + "file1.bob", tempDir + "file2.bob", tempDir + "file3.bob"), List("a" -> "1", "b" -> "2")))
        }
      }
    }
  }

  "POST build request" when {

    "Executor returns a successful build" should {
      "return 'OK' with build details" in {
        //arrange
        val content =
          """
            |namespace com.ialekseev.core#ping
            | description : \"ping\"
            |
            | @webhook
            |    queryString: [\"ping\": \"{$ping}\"]
            |
            | @process
            |    <scala>
            |        1 + 1
            |    <end>
          """.stripMargin

        val post = parse(s"""{"content":"$content", "vars": [{"a": "1"}, {"b": "2"}]}""")

        //act
        Post("/sandbox/sources/compile", post) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildResponse].succeed shouldBe true
        }
      }
    }
  }

  "POST run request" when {

    "Executor returns a successful Run (for incoming request having text body)" should {
      "return 'OK' with run details" in {
        //arrange
        val content =
          """
            |namespace com.ialekseev.core#ping
            | description : \"ping\"
            |
            | @webhook
            |    queryString: [\"q\": \"{$q}\"]
            |
            | @process
            |    <scala>
            |        q
            |    <end>
          """.stripMargin

        val post = parse( s"""{"content":"$content", "vars": [], "run": {"uri": "com.ialekseev.core/ping", "method": "GET", "headers": {}, "queryString": {"q" : "777"} } }""")

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunResponse] should be(PostRunResponse(some("777"), Nil))
        }
      }
    }
  }
}
