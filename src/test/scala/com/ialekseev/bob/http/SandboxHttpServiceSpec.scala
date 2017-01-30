package com.ialekseev.bob.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.ialekseev.bob.{InputSource, BaseSpec}

class SandboxHttpServiceSpec extends SandboxHttpService with BaseSpec with ScalatestRouteTest {

  "Sandbox routing" when {

    "GET request to the root is being sent" should {
      "succeed with the files" in {
        //act
        Get("/") ~> createRoutes(List(InputSource("path1\\file1.bob", "content1", List.empty), InputSource("path2\\file2.bob", "content2", List.empty))) ~> check {

          //assert
          response.status should be(StatusCodes.OK)
          responseAs[List[String]] should be(List("path1\\file1.bob", "path2\\file2.bob"))
        }
      }
    }
  }
}
