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

      "the source string contains just one word that matter" should {
        "succeed with 'Identifier' token" in {
          //act
          val result = lexer.tokenize(" good ")

          //assert
          result.right.get should be(List(Token.Identifier("good", 1, 4)))
        }
      }

      "the source string contains short 'namespace' line" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize("namespace com#create")

          //assert
          result.right.get should be(List(
            Token.Keyword.`namespace`(0),
            Token.Identifier("com", 10, 3),
            Token.Delimiter.`#`(13),
            Token.Identifier("create", 14, 6)
          ))
        }
      }

      "the source string contains long 'namespace' line" should {
        "succeed with tokens" ignore {
          //act
          val result = lexer.tokenize("  namespace com.ialekseev#create")

          //assert
          result.right.get should be(List(
            Token.INDENT(0, 2),
            Token.Keyword.`namespace`(2),
            Token.Identifier("com", 12, 3),
            Token.Delimiter.`.`(15),
            Token.Identifier("ialekseev", 16, 9),
            Token.Delimiter.`#`(25),
            Token.Identifier("create", 26, 6)
          ))
        }
      }

      "the source is HUGE" should {
        "not fail with Stack Overflow" in {
          //act
          val result = lexer.tokenize("namespace " * 1000)

          //assert
          result.right.get should not be(empty)
        }
      }
    }
}
