package com.ialekseev.bob.run.http

import java.nio.file.{Path, Paths}
import akka.actor.Props
import akka.http.scaladsl.model.StatusCodes
import com.ialekseev.bob._
import com.ialekseev.bob.exec.{CompilerActor, EvaluatorActor, Executor}
import com.ialekseev.bob.exec.analyzer.DefaultAnalyzer
import WebhookHttpService._
import com.ialekseev.bob.exec.Executor.Build
import com.ialekseev.bob.run.{BuildStateActor, SourceStateActor}
import org.json4s.native.JsonMethods._

class WebhookHttpServiceIntegrationSpec extends WebhookHttpService with HttpServiceBaseSpec {
  val executionContext = system.dispatcher
  val sandboxPathPrefix = "sandbox"
  val hookPathPrefix = "hook"
  val sourceStateActor = system.actorOf(Props(new SourceStateActor()(executionContext)))
  val buildStateActor = system.actorOf(Props[BuildStateActor])

  val exec: Executor = new Executor {
    val analyzer = DefaultAnalyzer
    val compilerActor = system.actorOf(Props[CompilerActor])
    val evaluatorActor = system.actorOf(Props[EvaluatorActor])
  }

  val tempDir = Paths.get(System.getProperty("java.io.tmpdir"))
  def escape(path: Path) = path.toString.replace("\\", "\\\\")

  val builds: List[Build] = List.empty

  "PUT sources request" when {

    "IO saves sources & build update has NOT been requested" should {
      "return 'OK'" in {
        //arrange
        val put = parse(s""" {"dirs": [{ "path": "${escape(tempDir)}", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }], "updateBuilds": false  }""")

        //act
        Put(s"/sandbox/sources", put) ~> createRoutes(List(tempDir), builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutSourcesResponse] should be(PutSourcesResponse(List.empty))
        }
      }
    }

    "IO saves sources (removes sources & vars actually) & build update has NOT been requested" should {
      "remove sources and vars & return 'OK'" in {
        //arrange
        val put0 = parse(s""" {"dirs": [{ "path": "${escape(tempDir)}", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")
        Put(s"/sandbox/sources", put0) ~> createRoutes(List(tempDir), builds) ~> check { response.status should be(StatusCodes.OK)}

        //act
        val put = parse(s""" {"dirs": [{ "path": "${escape(tempDir)}", "sources": [], "vars": [] }] }""")
        Put(s"/sandbox/sources", put) ~> createRoutes(List(tempDir), builds) ~> check {response.status should be(StatusCodes.OK)}

        //assert
        Get(s"/sandbox/sources") ~> createRoutes(List(tempDir), builds) ~> check {
          response.status should be(StatusCodes.OK)
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(List(InputDirModel(tempDir.toString, List.empty, List.empty))))
        }
      }
    }

    //todo: implement
    "IO saves sources & build update HAS been requested" should {
      "return 'OK'" ignore {
        //arrange
        val put = parse(s""" {"dirs": [{ "path": "${escape(tempDir)}", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }], "updateBuilds": false  }""")

        //act
        Put(s"/sandbox/sources", put) ~> createRoutes(List(tempDir), builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PutSourcesResponse] should be(PutSourcesResponse(List.empty))
        }
      }
    }
  }

  "GET sources request" when {

    "IO returns file" should {
      "return 'OK' with the file" in {
        //arrange
        Put(s"/sandbox/sources", parse(s""" {"dirs": [{ "path": "${escape(tempDir)}", "sources": [{"name": "file1", "content": "content1"}, {"name": "file2", "content": "content2"}], "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}] }]  }""")) ~> createRoutes(List(tempDir), builds) ~> check { response.status should be(StatusCodes.OK) }


        //act
        Get(s"/sandbox/sources") ~> createRoutes(List(tempDir), builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(List(InputDirModel(tempDir.toString, List(InputSourceModel("file1", "content1"), InputSourceModel("file2", "content2")), List(Variable("a", "1"), Variable("b", "2"))))))
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
            |    body : {\"par1\": \"1\"}
            |
            | @process
            |   1 + 1
          """.stripMargin

        val post = parse(s"""{"content":"$content", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/build", post) ~> createRoutes(List(tempDir), builds) ~> check {

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
            |    1 + abc
          """.stripMargin

        val post = parse(s"""{"content":"$content", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/build", post) ~> createRoutes(List(tempDir), builds) ~> check {

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
          """|namespace com.ialekseev.core#ping
             | description : \"ping\"
             |
             | @webhook
             |    queryString: [\"ping\": \"{$ping}\"]
             |
             | @process
             |    1 + abc
          """.stripMargin

        val post = parse(s"""{"content":"$content", "vars": [{"name": "a", "value": "1"}, {"name": "b", "value": "2"}]}""")

        //act
        Post("/sandbox/sources/build", post) ~> createRoutes(List(tempDir), builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostBuildFailureResponse].errors.head.startOffset shouldBe 130
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
            |    q
          """.stripMargin

        val post = parse( s"""{"content":"$content", "vars": [], "run": {"uri": "com.ialekseev.core/ping", "method": "GET", "headers": {}, "queryString": {"q" : "777"} } }""")

        //act
        Post("/sandbox/sources/run", post) ~> createRoutes(List(tempDir), builds) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[PostRunSuccessResponse] shouldBe PostRunSuccessResponse("777")
        }
      }
    }
  }
}
