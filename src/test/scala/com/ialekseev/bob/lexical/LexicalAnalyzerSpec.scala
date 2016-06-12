package com.ialekseev.bob.lexical

import com.ialekseev.bob.BaseSpec

class LexicalAnalyzerSpec extends BaseSpec {
  val lexer = new AdHocLexicalAnalyzer()

    "Doing lexical analysis" when {

      "source string is empty" should {
        "succeed with NO tokens" in {
          //act
          val result = lexer.tokenize("")

          //assert
          result.right.get should be(empty)
        }
      }

      "source string contains just whitespaces and newlines" should {
        "succeed with NO tokens" in {
          //act
          val result = lexer.tokenize("    \n     \n     ")

          //assert
          result.right.get should be(empty)
        }
      }

      "source string contains unknown word" should {
        "fail with one error" in {
          //act
          val result = lexer.tokenize("bad")

          //assert
          result.left.get should be(LexicalAnalysisError(0, 2))
        }
      }
    }
}
