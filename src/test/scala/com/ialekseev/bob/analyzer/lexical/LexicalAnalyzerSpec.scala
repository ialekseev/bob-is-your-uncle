package com.ialekseev.bob.analyzer.lexical

import com.ialekseev.bob.Models._
import com.ialekseev.bob.BaseSpec
import com.ialekseev.bob.analyzer.{LexerToken, Token}
import org.json4s.JsonDSL._

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
            LexerToken(Token.Type.StringLiteral("super"), 37)
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
            LexerToken(Token.Type.StringLiteral("super"), 14),
            LexerToken(Token.INDENT(0), 22),
            LexerToken(Token.Variable("header"), 22),
            LexerToken(Token.Delimiter.`:`, 29),
            LexerToken(Token.Type.StringLiteral("hello"), 31),
            LexerToken(Token.INDENT(2), 40),
            LexerToken(Token.Variable("createMeUri"), 42),
            LexerToken(Token.Delimiter.`:`, 54),
            LexerToken(Token.Type.StringLiteral("http://example.com/1"), 56)
          ))
        }
      }

      "the source string contains '@webhook' block" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(
            """@webhook """ + "\n\n  \n\n" +
              """ method : "get"""" + "\n" +
              """ queryString: ["a":"1", "b":"2"]""" + "\n" +
              """body: ~{"c": "3", "d": "hi!"}~""" + "\n" +
              """  headers : ["h1": "a", "h2": "b"]"""
          )

          //assert
          result.toEither.right.get should be(List(
            LexerToken(Token.INDENT(0), 0),
            LexerToken(Token.Keyword.`@webhook`, 0),

            LexerToken(Token.INDENT(1), 15),
            LexerToken(Token.Keyword.`method`, 16),
            LexerToken(Token.Delimiter.`:`, 23),
            LexerToken(Token.Type.StringLiteral("get"), 25),

            LexerToken(Token.INDENT(1), 31),
            LexerToken(Token.Keyword.`queryString`, 32),
            LexerToken(Token.Delimiter.`:`, 43),
            LexerToken(Token.Type.Dictionary("""["a":"1", "b":"2"]""", Map("a" -> "1", "b"->"2")), 45),

            LexerToken(Token.INDENT(0), 64),
            LexerToken(Token.Keyword.`body`, 64),
            LexerToken(Token.Delimiter.`:`, 68),
            LexerToken(Token.Type.Json("""~{"c": "3", "d": "hi!"}~""", ("c" -> "3") ~ ("d" -> "hi!") ), 70),

            LexerToken(Token.INDENT(2), 95),
            LexerToken(Token.Keyword.`headers`, 97),
            LexerToken(Token.Delimiter.`:`, 105),
            LexerToken(Token.Type.Dictionary("""["h1": "a", "h2": "b"]""", Map("h1" -> "a", "h2"->"b")), 107)
          ))
        }
      }

      "the source string contains @process" should {
        "succeed with tokens" in {
          //act
          val result = lexer.tokenize(
            """ @process""" + "\n" +
            """  <scala>""" + "\n" +
            """ val a = 4 > 3""" + "\n" +
            """<end>"""
          )

          //assert
          result.toEither.right.get should be(List(
            LexerToken(Token.INDENT(1), 0),
            LexerToken(Token.Keyword.`@process`, 1),
            LexerToken(Token.INDENT(2), 10),
            LexerToken(Token.Block.`<scala>`("\n val a = 4 > 3\n"), 12)
          ))
        }
      }

      "the source string has an error in 'namespace' line" should {
        "fail" in {
          //act
          val result = lexer.tokenize(
            """namespace com%%%#create""" + "\n" +
            """  description : "super"""")

          //assert
          result.toEither.left.get.errors should be(List(
            LexicalError(10, 15)
          ))
        }
      }

      "the source string has several errors in 'namespace' line" should {
        "fail" in {
          //act
          val result = lexer.tokenize(
            """%name%space % com#create%""" + "\n" +
              """  description : "super"""")

          //assert
          result.toEither.left.get.errors should be(List(
            LexicalError(0, 10),
            LexicalError(12, 12),
            LexicalError(18, 24)
          ))
        }
      }

      "the source string has errors in variables" should {
        "fail" in {
          //act
          val result = lexer.tokenize(
            """ ^description: "super"""" + "^\n" +
              """$hea^der: "hello"""" + "\r\n" +
              "\t" + """ $createMeUri: "http://example.com/1"""")

          //assert
          result.toEither.left.get.errors should be(List(
            LexicalError(1, 12),
            LexicalError(22, 22),
            LexicalError(24, 31)
          ))
        }
      }

      "the source string has an error in queryString's dictionary" should {
        "fail" in {
          //act
          val result = lexer.tokenize(
            """@webhook""" + "\n" +
              """method:"get"""" + "\n" +
              """queryString:["a": {"c":"3"}, "b":"2"]""")

          //assert
          result.toEither.left.get.errors.head.startOffset should be(34)
        }
      }

      "the source string has an error in header's dictionary" should {
        "fail" in {
          //act
          val result = lexer.tokenize(
            """@webhook""" + "\n" +
              """method:"get"""" + "\n" +
              """header:["a": {"c":"3"}, "b":"2"]""")

          //assert
          result.toEither.left.get.errors.head.startOffset should be (29)
        }
      }

      "the source string has an error in body's json" should {
        "fail" in {
          //act
          val result = lexer.tokenize(
            """@webhook""" + "\n" +
              """method:"get"""" + "\n" +
              """body: ~{"a": ["c":"3"}, "b":"2"}~""")

          //assert
          result.toEither.left.get.errors.head.startOffset should be (28)
        }
      }

      "the source string has an error in @process (no end of the block)" should {
        "fail" in {
          //act
          val result = lexer.tokenize(
            """ @process""" + "\n" +
              """  <scala>""" + "\n" +
              """ val a = 4 > 3"""
          )

          //assert
          result.toEither.left.get.errors.head.startOffset should be (12)
        }
      }

      "the source is HUGE" should {
        "NOT fail with Stack Overflow" in {
          //act
          val result = lexer.tokenize("namespace " * 1000)

          //assert
          result.toEither.right.get should not be(empty)
        }
      }
    }
}
