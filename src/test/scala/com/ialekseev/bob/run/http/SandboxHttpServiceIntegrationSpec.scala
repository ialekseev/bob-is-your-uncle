package com.ialekseev.bob.run.http

import java.net.URLEncoder
import java.nio.file.Paths

import akka.actor.Props
import akka.http.scaladsl.model.StatusCodes
import com.ialekseev.bob._
import com.ialekseev.bob.exec.{CompilerActor, EvaluatorActor, Executor}
import com.ialekseev.bob.exec.analyzer.DefaultAnalyzer
import com.ialekseev.bob.run.boot.HttpServiceUnsafe
import com.ialekseev.bob.run.http.SandboxHttpService._
import org.json4s.native.JsonMethods._

class SandboxHttpServiceIntegrationSpec extends SandboxHttpService with HttpServiceUnsafe with HttpServiceBaseSpec {
  val exec: Executor = new Executor {
    val analyzer = DefaultAnalyzer
    val compilerActor = system.actorOf(Props[CompilerActor])
    val evaluatorActor = system.actorOf(Props[EvaluatorActor])
  }
  val tempDir = Paths.get(System.getProperty("java.io.tmpdir"))
  val tempDirEncoded = URLEncoder.encode(tempDir.toString, "UTF-8")
  val varsFilePath = getVarsFilePath(tempDir)

  "PUT sources request" when {

    "IO updates sources" should {
      "return 'OK'" in {
        //arrange
        val put = parse(""" {"dirs": [{ "path": "\\test\\", "sources": [{"path": "\test\file1.bob", "content": "content1"}, {"path": "\test\file2.bob", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")

        //act
        Put(s"/sandbox/sources", put) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
        }
      }
    }
  }

  "GET sources request" when {

    "IO returns file" should {
      "return 'OK' with the file" in {
        //arrange
        Put(s"/sandbox/sources", parse(""" {"dirs": [{ "path": "\\test\\", "sources": [{"path": "\test\file1.bob", "content": "content1"}, {"path": "\test\file2.bob", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")) ~> createRoutes(tempDir) ~> check { response.status should be(StatusCodes.OK) }


        //act
        Get(s"/sandbox/sources") ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(List(InputDirModel("\\test\\", List(InputSourceModel("\\test\\file1.bob", "content1"), InputSourceModel("\\test\\file2.bob", "content2")), List(Variable("a", "1"), Variable("b", "2"))))))
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

        val post = parse(s"""{"content":"$content", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/compile", post) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildSuccessResponse.type] shouldBe PostBuildSuccessResponse
        }
      }
    }

    "Executor returns an analyzer failure" should {
      "return 'OK' with errors" in {
        //arrange
        val content =
          """
            |namespace com.ialekseev.core#ping
            | bad : \"ping\"
            |
            | @webhook
            |    queryString: [\"ping\": \"{$ping}\"]
            |
            | @process
            |    <scala>
            |        1 + abc
            |    <end>
          """.stripMargin

        val post = parse(s"""{"content":"$content", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/compile", post) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildFailureResponse].errors.head.startOffset shouldBe 38
          responseAs[PostBuildFailureResponse].errors.head.endOffset shouldBe 40
        }
      }
    }

    "Executor returns a compiler failure" should {
      "return 'OK' with errors" in {
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
            |        1 + abc
            |    <end>
          """.stripMargin

        val post = parse(s"""{"content":"$content", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/compile", post) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildFailureResponse].errors.head.startOffset shouldBe 149
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
          responseAs[PostRunSuccessResponse] shouldBe PostRunSuccessResponse("777")
        }
      }
    }
  }
}
