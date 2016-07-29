package com.ialekseev.bob.exec

import com.ialekseev.bob.BaseSpec

class ScalaCompilerSpec extends BaseSpec {
  "Analyzing" when {

    "lexer returns errors" should {
      "fail with lexical error" in {
        //act
        ScalaCompiler.compile("vak a = 1")

        //assert
      }
    }
  }
}
