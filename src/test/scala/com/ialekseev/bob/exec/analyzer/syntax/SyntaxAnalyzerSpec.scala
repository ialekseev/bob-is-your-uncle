package com.ialekseev.bob.exec.analyzer.syntax

import com.ialekseev.bob._
import com.ialekseev.bob.BaseSpec
import com.ialekseev.bob.exec.analyzer.syntax.SyntaxAnalyzer._
import com.ialekseev.bob.exec.analyzer.{LexerToken, Token}
import org.json4s.JsonAST.{JObject, JString}
import scalaz.Scalaz._
import scalaz._

class SyntaxAnalyzerSpec extends BaseSpec {
  val parser = new AdHocSyntaxAnalyzer()

  "Doing syntax analysis" when {

    "there is a min valid set of lexer tokens" should {
      "succeed" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 100),
          LexerToken(Token.Identifier("com"), 200),
          LexerToken(Token.Delimiter.`#`, 300),
          LexerToken(Token.Identifier("create"), 400),

          LexerToken(Token.INDENT(3), 450),
          LexerToken(Token.Keyword.`description`, 500),
          LexerToken(Token.Delimiter.`:`, 600),
          LexerToken(Token.Type.StringLiteral("hello"), 700),

          LexerToken(Token.INDENT(3), 750),
          LexerToken(Token.Keyword.`@webhook`, 800),

          LexerToken(Token.INDENT(3), 1150),
          LexerToken(Token.Block.`@process`("val a = 1"), 1200)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.right.get.drawTree should be ({
          nonTerminal("TopStat").node(
            nonTerminal("Namespace").node(
              terminal(LexerToken(Token.Keyword.`namespace`, 100)).leaf,
              nonTerminal("NamespacePath").node(
                terminal(LexerToken(Token.Identifier("com"), 200)).leaf
              ),
              terminal(LexerToken(Token.Delimiter.`#`, 300)).leaf,
              terminal(LexerToken(Token.Identifier("create"), 400)).leaf
            ),
            nonTerminal("Rule").node(
              nonTerminal("Description").node(
                terminal(LexerToken(Token.Keyword.`description`, 500)).leaf,
                terminal(LexerToken(Token.Delimiter.`:`, 600)).leaf,
                terminal(LexerToken(Token.Type.StringLiteral("hello"), 700)).leaf
              ),
              nonTerminal("Webhook").node(
                terminal(LexerToken(Token.Keyword.`@webhook`, 800)).leaf
              ),
              nonTerminal("Block").node(
                terminal(LexerToken(Token.Block.`@process`("val a = 1"), 1200)).leaf
              )
            )
          )
        }.drawTree)
      }
    }

    "there is a max valid set of lexer tokens" should {
      "succeed" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 100),
          LexerToken(Token.Identifier("com"), 200),
          LexerToken(Token.Delimiter.`.`, 300),
          LexerToken(Token.Identifier("ialekseev"), 400),
          LexerToken(Token.Delimiter.`#`, 500),
          LexerToken(Token.Identifier("create"), 600),

          LexerToken(Token.INDENT(3), 650),
          LexerToken(Token.Keyword.`description`, 700),
          LexerToken(Token.Delimiter.`:`, 800),
          LexerToken(Token.Type.StringLiteral("hello"), 900),

          LexerToken(Token.INDENT(3), 950),
          LexerToken(Token.Variable("var1"), 1000),
          LexerToken(Token.Delimiter.`:`, 1100),
          LexerToken(Token.Type.StringLiteral("alice"), 1200),

          LexerToken(Token.INDENT(3), 1250),
          LexerToken(Token.Variable("var2"), 1300),
          LexerToken(Token.Delimiter.`:`, 1400),
          LexerToken(Token.Type.StringLiteral("wonderland"), 1500),

          LexerToken(Token.INDENT(3), 1550),
          LexerToken(Token.Keyword.`@webhook`, 1600),

            LexerToken(Token.INDENT(5), 1650),
            LexerToken(Token.Keyword.`uri`, 1700),
            LexerToken(Token.Delimiter.`:`, 1800),
            LexerToken(Token.Type.StringLiteral("/example"), 1900),

            LexerToken(Token.INDENT(5), 1950),
            LexerToken(Token.Keyword.`method`, 2000),
            LexerToken(Token.Delimiter.`:`, 2100),
            LexerToken(Token.Type.StringLiteral("get"), 2200),

            LexerToken(Token.INDENT(5), 2250),
            LexerToken(Token.Keyword.`queryString`, 2300),
            LexerToken(Token.Delimiter.`:`, 2400),
            LexerToken(Token.Type.Dictionary("""["b":"18"]""", Map("b"->"18")), 2500),

            LexerToken(Token.INDENT(5), 2550),
            LexerToken(Token.Keyword.`body`, 2600),
            LexerToken(Token.Delimiter.`:`, 2700),
            LexerToken(Token.Type.Json("""~{"c":"19"}~""", JObject("c"-> JString("19"))), 2800),

            LexerToken(Token.INDENT(5), 2850),
            LexerToken(Token.Keyword.`headers`, 2900),
            LexerToken(Token.Delimiter.`:`, 3000),
            LexerToken(Token.Type.Dictionary("""["h1":"a"]""", Map("h1"->"a")), 3100),

          LexerToken(Token.INDENT(3), 3150),
          LexerToken(Token.Block.`@process`("val a = 1"), 3200)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.right.get.drawTree should be ({
          nonTerminal("TopStat").node(
            nonTerminal("Namespace").node(
              terminal(LexerToken(Token.Keyword.`namespace`, 100)).leaf,
              nonTerminal("NamespacePath").node(
                terminal(LexerToken(Token.Identifier("com"), 200)).leaf,
                nonTerminal("NamespacePathParts").node(
                  nonTerminal("NamespacePathPart").node(
                    terminal(LexerToken(Token.Delimiter.`.`, 300)).leaf,
                    terminal(LexerToken(Token.Identifier("ialekseev"), 400)).leaf
                  )
                )
              ),
              terminal(LexerToken(Token.Delimiter.`#`, 500)).leaf,
              terminal(LexerToken(Token.Identifier("create"),600)).leaf
            ),
            nonTerminal("Rule").node(
              nonTerminal("Description").node(
                terminal(LexerToken(Token.Keyword.`description`, 700)).leaf,
                terminal(LexerToken(Token.Delimiter.`:`, 800)).leaf,
                terminal(LexerToken(Token.Type.StringLiteral("hello"), 900)).leaf
              ),
              nonTerminal("Constants").node(
                nonTerminal("Constant").node(
                  terminal(LexerToken(Token.Variable("var1"), 1000)).leaf,
                  terminal(LexerToken(Token.Delimiter.`:`, 1100)).leaf,
                  terminal(LexerToken(Token.Type.StringLiteral("alice"), 1200)).leaf
                ),
                nonTerminal("Constant").node(
                  terminal(LexerToken(Token.Variable("var2"), 1300)).leaf,
                  terminal(LexerToken(Token.Delimiter.`:`, 1400)).leaf,
                  terminal(LexerToken(Token.Type.StringLiteral("wonderland"), 1500)).leaf
                )
              ),
              nonTerminal("Webhook").node(
                terminal(LexerToken(Token.Keyword.`@webhook`, 1600)).leaf,
                nonTerminal("WebhookSettings").node(
                  nonTerminal("WebhookSetting").node(
                    terminal(LexerToken(Token.Keyword.`uri`, 1700)).leaf,
                    terminal(LexerToken(Token.Delimiter.`:`, 1800)).leaf,
                    terminal(LexerToken(Token.Type.StringLiteral("/example"), 1900)).leaf
                  ),
                  nonTerminal("WebhookSetting").node(
                    terminal(LexerToken(Token.Keyword.`method`, 2000)).leaf,
                    terminal(LexerToken(Token.Delimiter.`:`, 2100)).leaf,
                    terminal(LexerToken(Token.Type.StringLiteral("get"), 2200)).leaf
                  ),
                  nonTerminal("WebhookSetting").node(
                    terminal(LexerToken(Token.Keyword.`queryString`, 2300)).leaf,
                    terminal(LexerToken(Token.Delimiter.`:`, 2400)).leaf,
                    terminal(LexerToken(Token.Type.Dictionary("""["b":"18"]""", Map("b"->"18")), 2500)).leaf
                  ),
                  nonTerminal("WebhookSetting").node(
                    terminal(LexerToken(Token.Keyword.`body`, 2600)).leaf,
                    terminal(LexerToken(Token.Delimiter.`:`, 2700)).leaf,
                    nonTerminal("WebhookSettingBodyType").node(
                      terminal(LexerToken(Token.Type.Json("""~{"c":"19"}~""", JObject("c"-> JString("19"))), 2800)).leaf
                    )
                  ),
                  nonTerminal("WebhookSetting").node(
                    terminal(LexerToken(Token.Keyword.`headers`, 2900)).leaf,
                    terminal(LexerToken(Token.Delimiter.`:`, 3000)).leaf,
                    terminal(LexerToken(Token.Type.Dictionary("""["h1":"a"]""", Map("h1"->"a")), 3100)).leaf
                  )
                )
              ),
              nonTerminal("Block").node(
                terminal(LexerToken(Token.Block.`@process`("val a = 1"), 3200)).leaf
              )
            )
          )
        }.drawTree)
      }
    }

    "there is an error (an invalid delimiter in namespace)" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`:`, 13),
          LexerToken(Token.Identifier("create"), 14)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(13, 13, 3, "Unexpected: ':' (expecting: '#')")))
      }
    }

    "there is an error (no indent before 'description')" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.Keyword.`description`, 23)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(23, 33, 5, "Unexpected: 'description' (expecting: 'indent')")))
      }
    }

    "there is an error (got an identifier instead of the string literal in the description)" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.Identifier("com"), 35)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(35, 37, 8, "Unexpected: 'com' (expecting: 'string literal')")))
      }
    }

    "there is an error (an invalid delimiter in the first constant line)" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 16),
          LexerToken(Token.Keyword.`description`, 18),
          LexerToken(Token.Delimiter.`:`, 20),
          LexerToken(Token.Type.StringLiteral("hello"), 22),

          LexerToken(Token.INDENT(3), 24),
          LexerToken(Token.Variable("var1"), 26),
          LexerToken(Token.Delimiter.`#`, 28),
          LexerToken(Token.Type.StringLiteral("alice"), 30)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(28, 28, 11, "Unexpected: '#' (expecting: ':')")))
      }
    }

    "there is an error (an invalid type in the second constant line)" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 16),
          LexerToken(Token.Keyword.`description`, 18),
          LexerToken(Token.Delimiter.`:`, 20),
          LexerToken(Token.Type.StringLiteral("hello"), 22),

          LexerToken(Token.INDENT(3), 24),
          LexerToken(Token.Variable("var1"), 26),
          LexerToken(Token.Delimiter.`:`, 28),
          LexerToken(Token.Type.StringLiteral("alice"), 30),

          LexerToken(Token.INDENT(3), 32),
          LexerToken(Token.Variable("var2"), 34),
          LexerToken(Token.Delimiter.`:`, 36),
          LexerToken(Token.Type.Dictionary("[b:11]", Map("b" -> "11")), 38)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(38, 43, 16, "Unexpected: 'Dictionary([b:11],Map(b -> 11))' (expecting: 'string literal')")))
      }
    }

    "there is an error (an invalid indent before '@webhook')" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.Type.StringLiteral("hello"), 35),

          LexerToken(Token.INDENT(2), 40),
          LexerToken(Token.Keyword.`@webhook`, 45)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(40, 41, 9, "Unexpected indent width: 2. Expected: 3")))
      }
    }

    "there is an error (an invalid type in the 'uri' line inside '@webhook')" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.Type.StringLiteral("hello"), 35),

          LexerToken(Token.INDENT(3), 40),
          LexerToken(Token.Keyword.`@webhook`, 45),

          LexerToken(Token.INDENT(5), 50),
          LexerToken(Token.Keyword.`uri`, 55),
          LexerToken(Token.Delimiter.`:`, 56),
          LexerToken(Token.Type.Dictionary("a = 1", Map("a" -> "1")), 61)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(61, 65, 14, "Unexpected: 'Dictionary(a = 1,Map(a -> 1))' (expecting: 'string literal')")))
      }
    }

    "there is an error (an invalid delimiter in the 'method' line inside '@webhook')" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.Type.StringLiteral("hello"), 35),

          LexerToken(Token.INDENT(3), 40),
          LexerToken(Token.Keyword.`@webhook`, 45),

          LexerToken(Token.INDENT(5), 50),
          LexerToken(Token.Keyword.`uri`, 55),
          LexerToken(Token.Delimiter.`:`, 56),
          LexerToken(Token.Type.StringLiteral("/example"), 61),

          LexerToken(Token.INDENT(5), 62),
          LexerToken(Token.Keyword.`method`, 65),
          LexerToken(Token.Delimiter.`#`, 66),
          LexerToken(Token.Type.StringLiteral("get"), 69)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(66, 66, 17, "Unexpected: '#' (expecting: ':')")))
      }
    }

    "there is an error (an invalid type in the 'queryString' line inside '@webhook')" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.Type.StringLiteral("hello"), 35),

          LexerToken(Token.INDENT(3), 40),
          LexerToken(Token.Keyword.`@webhook`, 45),

          LexerToken(Token.INDENT(5), 50),
          LexerToken(Token.Keyword.`uri`, 55),
          LexerToken(Token.Delimiter.`:`, 56),
          LexerToken(Token.Type.StringLiteral("/example"), 61),

          LexerToken(Token.INDENT(5), 62),
          LexerToken(Token.Keyword.`method`, 65),
          LexerToken(Token.Delimiter.`:`, 66),
          LexerToken(Token.Type.StringLiteral("get"), 69),

          LexerToken(Token.INDENT(5), 72),
          LexerToken(Token.Keyword.`queryString`, 75),
          LexerToken(Token.Delimiter.`:`, 76),
          LexerToken(Token.Type.StringLiteral("/example"), 79)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(79, 88, 22, """Unexpected: '"/example"' (expecting: 'dictionary')""")))
      }
    }

    "there is an error (an invalid token in the 'body' line inside '@webhook')" should {
      "fail" in {
        //arrange
        val tokens = List(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.Type.StringLiteral("hello"), 35),

          LexerToken(Token.INDENT(3), 40),
          LexerToken(Token.Keyword.`@webhook`, 45),

          LexerToken(Token.INDENT(5), 50),
          LexerToken(Token.Keyword.`uri`, 55),
          LexerToken(Token.Delimiter.`:`, 56),
          LexerToken(Token.Type.StringLiteral("/example"), 61),

          LexerToken(Token.INDENT(5), 62),
          LexerToken(Token.Keyword.`body`, 65),
          LexerToken(Token.Delimiter.`:`, 66),
          LexerToken(Token.Identifier("super"), 69),

          LexerToken(Token.INDENT(5), 72),
          LexerToken(Token.Keyword.`queryString`, 75),
          LexerToken(Token.Delimiter.`:`, 76),
          LexerToken(Token.Type.StringLiteral("/example"), 79)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get.errors should be (List(SyntaxError(69, 73, 18, "Expecting some valid Body type here")))
      }
    }
  }
}
