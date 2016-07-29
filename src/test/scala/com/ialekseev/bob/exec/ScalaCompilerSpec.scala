package com.ialekseev.bob.exec

import com.ialekseev.bob.BaseSpec

class ScalaCompilerSpec extends BaseSpec {
  "Analyzing" when {

    "lexer returns errors" should {
      "fail with lexical error" in {
        //act
        val result = ScalaCompiler.compile("vak a = 1")

        //assert
        //todo: complete
        result should be (111)
      }
    }
  }
}
