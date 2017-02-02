package com.ialekseev.bob.http

import java.io.{FileNotFoundException}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.ialekseev.bob.BaseSpec
import com.ialekseev.bob.Models.InputSource
import com.ialekseev.bob.exec.Executor
import scalaz._
import Scalaz._
import org.mockito.Mockito._

class SandboxHttpServiceSpec extends SandboxHttpService with BaseSpec with ScalatestRouteTest {
  val sandboxExecutor: Executor = mock[Executor]
  override def beforeEach(): Unit = { reset(sandboxExecutor); super.beforeEach()}

  private var listFilesFunc: String => IoTry[List[String]] = null
  private var readSourceFunc: String => IoTry[InputSource] = null

  override def listFiles(dir: String): IoTry[List[String]] = listFilesFunc(dir)
  override def readSource(filename: String): IoTry[InputSource] = readSourceFunc(filename)

  "GET list of all files request" when {

    "IO returns list of files" should {
      "return 'OK' with the files" in {
        //arrange
        listFilesFunc = {
          case "\\test\\" => IoTry(List("\\test\\file1.bob", "\\test\\file2.bob").right)
        }

        //act
        Get("/sandbox/sources") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[List[String]] should be(List("\\test\\file1.bob", "\\test\\file2.bob"))
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        listFilesFunc = {
          case "\\test\\" => IoTry(List(new FileNotFoundException("bad!")).left)
        }

        //act
        Get("/sandbox/sources") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
          responseAs[List[String]] should be (List("bad!"))
        }
      }
    }
  }

  "GET one source request" when {

    "IO returns file" should {
      "return 'OK' with the files" in {
        //arrange
        readSourceFunc = {
          case "\\test\\file1.bob" => IoTry(InputSource("\\test\\file1.bob", "content", List("a" -> "1", "b" -> "2")).right)
        }

        //act
        Get("/sandbox/sources/%5Ctest%5Cfile1.bob") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[InputSource] should be(InputSource("\\test\\file1.bob", "content", List("a" -> "1", "b" -> "2")))
        }
      }
    }

    "IO fails" should {
      "return 'InternalServerError'" in {
        //arrange
        readSourceFunc = {
          case "\\test\\file1.bob" => IoTry(List(new FileNotFoundException("bad!")).left)
        }

        //act
        Get("/sandbox/sources/%5Ctest%5Cfile1.bob") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
          responseAs[List[String]] should be (List("bad!"))
        }
      }
    }
  }
}
