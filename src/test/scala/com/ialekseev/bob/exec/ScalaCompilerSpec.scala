package com.ialekseev.bob.exec

import com.ialekseev.bob.BaseSpec
import scalaz._
import Scalaz._

class ScalaCompilerSpec extends BaseSpec {
  "Compiling" when {

    "code has an error" should {
      "fail" in {
        //arrange
        val compiler = new ScalaCompiler()

        //act
        val result = compiler.compile("vak = 3")

        //assert
        (result.toEither.left.get.errors.head |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (0,0,0)
      }
    }

    "code has several errors" should {
      "fail" in {
        //arrange
        val compiler = new ScalaCompiler()

        //act
        val result = compiler.compile("vak = 3; a.call()")

        //assert
        result.toEither.left.get.errors.length should be (2)
        (result.toEither.left.get.errors(0) |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (0,0,0)
        (result.toEither.left.get.errors(1) |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (9,9,9)
      }
    }
  }
}
