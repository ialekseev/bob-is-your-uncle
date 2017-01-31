package com.ialekseev.bob.http

import java.io.{FileNotFoundException, File}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.ialekseev.bob.BaseSpec
import scalaz._
import Scalaz._

class SandboxHttpServiceSpec extends SandboxHttpService with BaseSpec with ScalatestRouteTest {
  var listFilesFunc: PartialFunction[String, IoTry[List[File]]] = {
    case _ => IoTry(List.empty.right)
  }

  override def listFiles(dir: String): IoTry[List[File]] = listFilesFunc(dir)

  "GET '/' request" when {

    "listFiles return files" should {
      "return 'OK' with the files" in {
        //arrange
        listFilesFunc = {
          case "\\test\\" => IoTry(List(new File("\\test\\file1.bob"), new File("\\test\\file2.bob")).right)
        }

        //act
        Get("/") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[List[String]] should be(List("\\test\\file1.bob", "\\test\\file2.bob"))
        }
      }
    }

    "listFiles fails" should {
      "return 'InternalServerError'" in {
        //arrange
        listFilesFunc = {
          case "\\test\\" => IoTry(List(new FileNotFoundException("bad!")).left)
        }

        //act
        Get("/") ~> createRoutes("\\test\\") ~> check {

          //assert
          response.status should be(StatusCodes.InternalServerError)
          responseAs[List[String]] should be (List("bad!"))
        }
      }
    }
  }
}
