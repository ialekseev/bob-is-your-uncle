package com.ialekseev.bob.http

import java.io.{FileNotFoundException}
import akka.http.scaladsl.model.{StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.ialekseev.bob.BaseSpec
import com.ialekseev.bob.exec.Executor
import com.ialekseev.bob.http.SandboxHttpService.{PutOneSourceResponse, GetOneSourceResponse, GetSourcesResponse}
import org.mockito.Mockito._
import org.json4s.native.JsonMethods._

class SandboxHttpServiceSpec extends SandboxHttpService with BaseSpec with ScalatestRouteTest {
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
          responseAs[GetSourcesResponse] should be(GetSourcesResponse(List("\\test\\file1.bob", "\\test\\file2.bob"), Map("a" -> "1", "b" -> "2")))
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
          responseAs[List[String]] should be (List("bad list!"))
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
          responseAs[List[String]] should be (List("bad var!"))
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
          val test1 = response
          test1.status should be(StatusCodes.InternalServerError)
          responseAs[List[String]] should be (List("bad!"))
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
          responseAs[List[String]] should be (List("bad!"))
        }
      }
    }
  }
}
