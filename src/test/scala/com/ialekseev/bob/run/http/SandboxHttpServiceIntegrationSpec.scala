package com.ialekseev.bob.run.http

import java.net.URLEncoder

import akka.actor.Props
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
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
  val tempDir = normalizeDirPath(System.getProperty("java.io.tmpdir"))
  val tempDirEncoded = URLEncoder.encode(tempDir, "UTF-8")
  val varsFilePath = getVarsFilePath(tempDir)

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
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(tempDir, List(tempDir + "file1.bob", tempDir + "file2.bob", tempDir + "file3.bob"), List(Variable("a", "1"), Variable("b", "2"))))
        }
      }
    }
  }

  "PUT vars request" when {

    "IO updates vars file (when exists)" should {
      "return 'OK' with the file" in {
        //arrange
        updateFile(varsFilePath, "{}").unsafePerformSync

        assume(findVarsFile(tempDir).unsafePerformSync.isDefined, "Vars file should exist for this test to make sense")

        val put = parse("""{"vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Put(s"/sandbox/sources/vars/${tempDirEncoded}", put) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutVarsResponse] should be(PutVarsResponse(tempDir + "_vars.json"))
        }
      }
    }

    "IO creates vars file (when doesn't exist)" should {
      "return 'OK' with the file" in {
        //arrange
        deleteIfExists(varsFilePath).unsafePerformSync

        assume(findVarsFile(tempDir).unsafePerformSync.isEmpty, "Vars file should NOT exist for this test to make sense")

        val put = parse("""{"vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Put(s"/sandbox/sources/vars/${tempDirEncoded}", put) ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutVarsResponse] should be(PutVarsResponse(tempDir + "_vars.json"))
        }
      }
    }

    "passing wrong JSON (array without 'vars' wrapping)" should {
      "return 'OK' with the file. But all variables in the file have been erased. This is json4s's behaviour of not failing when there is no matching field: https://github.com/json4s/json4s/issues/375" in {
        //arrange
        val put = parse("""[{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]""")

        //act
        Put(s"/sandbox/sources/vars/${tempDirEncoded}", put) ~> Route.seal(createRoutes(tempDir)) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutVarsResponse] should be(PutVarsResponse(tempDir + "_vars.json"))
        }

        Get("/sandbox/sources") ~> createRoutes(tempDir) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetSourcesResponse].vars should be(List.empty)
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
