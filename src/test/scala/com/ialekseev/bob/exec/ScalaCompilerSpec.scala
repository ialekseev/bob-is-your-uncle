package com.ialekseev.bob.exec

import com.ialekseev.bob.BaseSpec
import com.ialekseev.bob.run.Boot
import scalaz._
import Scalaz._

class ScalaCompilerSpec extends BaseSpec {
  val compiler = Boot.compiler

  "Compiling" when {

    "code has an error" should {
      "fail" in {
        //act
        val result = compiler.compile("vak = 3")

        //assert
        (result.toEither.left.get.errors.head |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (93,93,93)
      }
    }

    "code has several errors" should {
      "fail" in {
        //act
        val result = compiler.compile("a.call()", fields = "vak = 3;")

        //assert
        result.toEither.left.get.errors.length should be (2)
        (result.toEither.left.get.errors(0) |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (73,73,73)
        (result.toEither.left.get.errors(1) |> (a => (a.startOffset, a.pointOffset, a.endOffset))) should be (101,101,101)
      }
    }
  }

  "Evaluating" when {

    "there are NO passed in variables" should {
      "succeed" in {
        //arrange
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
        val className = compiler.compile("a + b", fields = """var a = ""; var b = """"").toEither.right.get

        //act
        val result = compiler.eval[String](className, Seq("a" -> "1", "b" -> "hi"))

        //assert
        result should be ("1hi")
      }
    }
  }
}
