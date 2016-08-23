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
        val result = compiler.compile("vak = 3", "")

        //assert
        (result.toEither.left.get.errors.head |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (90,90,90)
      }
    }

    "code has several errors" should {
      "fail" in {
        //arrange
        val compiler = new ScalaCompiler()

        //act
        val result = compiler.compile("a.call()", "vak = 3;")

        //assert
        result.toEither.left.get.errors.length should be (2)
        (result.toEither.left.get.errors(0) |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (72,72,72)
        (result.toEither.left.get.errors(1) |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (99,99,99)
      }
    }
  }

  "Evaluating" when {

    "there are NO passed in variables" should {
      "succeed" in {
        //arrange
        val compiler = new ScalaCompiler()
        val className = compiler.compile("1 + 1").toEither.right.get

        //act
        val result = compiler.eval[Int](className, Seq.empty)

        //assert
        result should be (2)
      }
    }

    "there ARE passed in variables" should {
      "succeed" in {
        //arrange
        val compiler = new ScalaCompiler()
        val className = compiler.compile("a + b", """var a = ""; var b = """"").toEither.right.get

        //act
        val result = compiler.eval[String](className, Seq("a" -> "1", "b" -> "hi"))

        //assert
        result should be ("1hi")
      }
    }
  }
}
