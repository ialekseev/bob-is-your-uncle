package com.ialekseev.bob.lexical

import com.ialekseev.bob.{BaseSpec, Token}

class LexicalAnalyzerSpec extends BaseSpec {
  val lexer = new AdHocLexicalAnalyzer()

    "Doing lexical analysis" when {

      "the source string is empty" should {
        "succeed with NO tokens" in {
          //act
          val result = lexer.tokenize("")

          //assert
          result.right.get should be(empty)
        }
      }

      "the source string contains just whitespaces and newlines" should {
        "succeed with NO tokens" in {
          //act
          val result = lexer.tokenize("    \n     \n     ")

          //assert
          result.right.get should be(empty)
        }
      }

      "the source string contains just one word" should {
        "succeed with Identifier token" in {
          //act
          val result = lexer.tokenize("bad")

          //assert
          result.right.get should be(List(Token.Identifier("bad", 0, 3)))
        }
      }
    }
}
