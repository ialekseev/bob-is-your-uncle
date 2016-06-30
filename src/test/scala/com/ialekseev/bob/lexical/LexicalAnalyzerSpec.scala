package com.ialekseev.bob.lexical

import com.ialekseev.bob.{BaseSpec, Token}
import com.ialekseev.bob.lexical.LexicalAnalyzer._

class LexicalAnalyzerSpec extends BaseSpec {
  val lexer = new AdHocLexicalAnalyzer()

    "Doing lexical analysis" when {

      "the source string is empty" should {
        "succeed with NO tokens" in {
          //act
          val result = lexer.tokenize("")

          //assert
          result.toEither.right.get should be(empty)
        }
      }

      "the source string contains just whitespaces and newlines" should {
        "succeed with NO tokens" in {
          //act
          val result = lexer.tokenize("    \n     \n     ")

          //assert
          result.toEither.right.get should be(empty)
        }
      }

      "the source string contains just one word that matter" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(" good ")

          //assert
          result.toEither.right.get should be(List(
            LexerToken(Token.INDENT(1), 0),
            LexerToken(Token.Identifier("good"), 1)
          ))
        }
      }

      "the source string contains short 'namespace'" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize("\n \n  namespace com#create\r")

          //assert
          result.toEither.right.get should be(List(
            LexerToken(Token.INDENT(2), 3),
            LexerToken(Token.Keyword.`namespace`, 5),
            LexerToken(Token.Identifier("com"), 15),
            LexerToken(Token.Delimiter.`#`, 18),
            LexerToken(Token.Identifier("create"), 19)
          ))
        }
      }

      "the source string contains long 'namespace'" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize("  namespace com.ialekseev#create")

          //assert
          result.toEither.right.get should be(List(
            LexerToken(Token.INDENT(2), 0),
            LexerToken(Token.Keyword.`namespace`, 2),
            LexerToken(Token.Identifier("com"), 12),
            LexerToken(Token.Delimiter.`.`, 15),
            LexerToken(Token.Identifier("ialekseev"), 16),
            LexerToken(Token.Delimiter.`#`, 25),
            LexerToken(Token.Identifier("create"), 26)
          ))
        }
      }

      "the source string contains 'description'" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(
            """namespace com#create""" + "\n" +
            """  description : "super"""")

          //assert
          result.toEither.right.get should be(List(
            LexerToken(Token.INDENT(0), 0),
            LexerToken(Token.Keyword.`namespace`, 0),
            LexerToken(Token.Identifier("com"), 10),
            LexerToken(Token.Delimiter.`#`, 13),
            LexerToken(Token.Identifier("create"), 14),
            LexerToken(Token.INDENT(2), 21),
            LexerToken(Token.Keyword.`description`, 23),
            LexerToken(Token.Delimiter.`:`, 35),
            LexerToken(Token.StringLiteral("super"), 37)
          ))
        }
      }

      "the source string contains variables" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(
            """ description: "super"""" + "\n" +
            """$header: "hello"""" + "\r\n" +
            "\t" + """ $createMeUri: "http://example.com/1"""")

          //assert
          result.toEither.right.get should be(List(
            LexerToken(Token.INDENT(1), 0),
            LexerToken(Token.Keyword.`description`, 1),
            LexerToken(Token.Delimiter.`:`, 12),
            LexerToken(Token.StringLiteral("super"), 14),
            LexerToken(Token.INDENT(0), 22),
            LexerToken(Token.Variable("header"), 22),
            LexerToken(Token.Delimiter.`:`, 29),
            LexerToken(Token.StringLiteral("hello"), 31),
            LexerToken(Token.INDENT(2), 40),
            LexerToken(Token.Variable("createMeUri"), 42),
            LexerToken(Token.Delimiter.`:`, 54),
            LexerToken(Token.StringLiteral("http://example.com/1"), 56)
          ))
        }
      }

      "the source string contains '@webhook' block" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(
            """@webhook """ + "\n\n  \n\n" +
              """ method : "get"""" + "\n" +
              """ queryString: "url/do"""")

          //assert
          result.toEither.right.get should be(List(
            LexerToken(Token.INDENT(0), 0),
            LexerToken(Token.Keyword.`@webhook`, 0),
            LexerToken(Token.INDENT(1), 15),
            LexerToken(Token.Keyword.`method`, 16),
            LexerToken(Token.Delimiter.`:`, 23),
            LexerToken(Token.StringLiteral("get"), 25),
            LexerToken(Token.INDENT(1), 31),
            LexerToken(Token.Keyword.`queryString`, 32),
            LexerToken(Token.Delimiter.`:`, 43),
            LexerToken(Token.StringLiteral("url/do"), 45)
          ))
        }
      }

      "the source string has an error in 'namespace' line" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(
            """namespace com%%%#create""" + "\n" +
            """  description : "super"""")

          //assert
          result.toEither.left.get should be(List(
            LexerError(10, 15)
          ))
        }
      }

      "the source string has several errors in 'namespace' line" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(
            """%name%space % com#create%""" + "\n" +
              """  description : "super"""")

          //assert
          result.toEither.left.get should be(List(
            LexerError(0, 5),
            LexerError(12, 12),
            LexerError(18, 24)
          ))
        }
      }

      "the source string has errors in variables" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(
            """ ^description: "super"""" + "^\n" +
              """$hea^der: "hello"""" + "\r\n" +
              "\t" + """ $createMeUri: "http://example.com/1"""")

          //assert
          result.toEither.left.get should be(List(
            LexerError(1, 1),
            LexerError(22, 22),
            LexerError(24, 28)
          ))
        }
      }

      "the source is HUGE" should {
        "not fail with Stack Overflow" in {
          //act
          val result = lexer.tokenize("namespace " * 1000)

          //assert
          result.toEither.right.get should not be(empty)
        }
      }
    }
}
