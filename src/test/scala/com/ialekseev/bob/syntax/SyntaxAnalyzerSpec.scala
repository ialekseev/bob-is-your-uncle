package com.ialekseev.bob.syntax

import com.ialekseev.bob.lexical.LexicalAnalyzer.LexerToken
import com.ialekseev.bob.syntax.SyntaxAnalyzer._
import com.ialekseev.bob.{Token, BaseSpec}
import scalaz._
import Scalaz._

class SyntaxAnalyzerSpec extends BaseSpec {
  val parser = new LL1SyntaxAnalyzer()

  "Doing syntax analysis" when {

    "there is a min valid set of lexer tokens" should {
      "succeed" in {
        //arrange
        val tokens = Seq(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.StringLiteral("hello"), 35),

          LexerToken(Token.INDENT(3), 40),
          LexerToken(Token.Keyword.`@webhook`, 45),

          LexerToken(Token.INDENT(5), 50),
          LexerToken(Token.Keyword.`uri`, 55),
          LexerToken(Token.Delimiter.`:`, 56),
          LexerToken(Token.StringLiteral("/example"), 61)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        implicitly[Equal[ParseTree]].equal(result.toEither.right.get, {
          nonTerminal("TopStat").node(
            nonTerminal("Namespace").node(
              terminal(LexerToken(Token.Keyword.`namespace`, 0)).leaf,
              nonTerminal("NamespacePath").node(
                terminal(LexerToken(Token.Identifier("com"), 10)).leaf
              ),
              terminal(LexerToken(Token.Delimiter.`#`, 13)).leaf,
              terminal(LexerToken(Token.Identifier("create"), 14)).leaf
            ),
            nonTerminal("Rule").node(
              nonTerminal("Description").node(
                terminal(LexerToken(Token.Keyword.`description`, 23)).leaf,
                terminal(LexerToken(Token.Delimiter.`:`, 26)).leaf,
                terminal(LexerToken(Token.StringLiteral("hello"), 35)).leaf
              ),
              nonTerminal("Webhook").node(
                terminal(LexerToken(Token.Keyword.`@webhook`, 45)).leaf,
                nonTerminal("WebhookSettings").node(
                  nonTerminal("WebhookUriSetting").node(
                    terminal(LexerToken(Token.Keyword.`uri`, 55)).leaf,
                    terminal(LexerToken(Token.Delimiter.`:`, 56)).leaf,
                    terminal(LexerToken(Token.StringLiteral("/example"), 61)).leaf
                  )
                )
              )
            )
          )
        }) should be (true)
      }
    }

    //todo: when there is an error in optional (repeated) block this error is swalloed. Fix this
    "there is a max valid set of lexer tokens" should {
      "succeed" in {
        //arrange
        val tokens = Seq(
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
          LexerToken(Token.StringLiteral("hello"), 900),

          LexerToken(Token.INDENT(3), 950),
          LexerToken(Token.Variable("var1"), 1000),
          LexerToken(Token.Delimiter.`:`, 1100),
          LexerToken(Token.StringLiteral("alice"), 1200),

          LexerToken(Token.INDENT(3), 1250),
          LexerToken(Token.Variable("var2"), 1300),
          LexerToken(Token.Delimiter.`:`, 1400),
          LexerToken(Token.StringLiteral("wonderland"), 1500),

          LexerToken(Token.INDENT(3), 1550),
          LexerToken(Token.Keyword.`@webhook`, 1600),

          LexerToken(Token.INDENT(5), 1650),
          LexerToken(Token.Keyword.`uri`, 1700),
          LexerToken(Token.Delimiter.`:`, 1800),
          LexerToken(Token.StringLiteral("/example"), 1900),

          LexerToken(Token.INDENT(5), 1950),
          LexerToken(Token.Keyword.`method`, 2000),
          LexerToken(Token.Delimiter.`:`, 2100),
          LexerToken(Token.StringLiteral("get"), 2200),

          LexerToken(Token.INDENT(5), 2250),
          LexerToken(Token.Keyword.`queryString`, 2300),
          LexerToken(Token.Delimiter.`:`, 2400),
          LexerToken(Token.StringLiteral("b=18"), 2500)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        implicitly[Equal[ParseTree]].equal(result.toEither.right.get, {
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
                terminal(LexerToken(Token.StringLiteral("hello"), 900)).leaf
              ),
              nonTerminal("Constants").node(
                nonTerminal("Constant").node(
                  terminal(LexerToken(Token.Variable("var1"), 1000)).leaf,
                  terminal(LexerToken(Token.Delimiter.`:`, 1100)).leaf,
                  terminal(LexerToken(Token.StringLiteral("alice"), 1200)).leaf
                ),
                nonTerminal("Constant").node(
                  terminal(LexerToken(Token.Variable("var2"), 1300)).leaf,
                  terminal(LexerToken(Token.Delimiter.`:`, 1400)).leaf,
                  terminal(LexerToken(Token.StringLiteral("wonderland"), 1500)).leaf
                )
              ),
              nonTerminal("Webhook").node(
                terminal(LexerToken(Token.Keyword.`@webhook`, 1600)).leaf,
                nonTerminal("WebhookSettings").node(
                  nonTerminal("WebhookUriSetting").node(
                    terminal(LexerToken(Token.Keyword.`uri`, 1700)).leaf,
                    terminal(LexerToken(Token.Delimiter.`:`, 1800)).leaf,
                    terminal(LexerToken(Token.StringLiteral("/example"), 1900)).leaf
                  ),
                  nonTerminal("WebhookSpecificSettings").node(
                    nonTerminal("WebhookSpecificSetting").node(
                      terminal(LexerToken(Token.Keyword.`method`, 2000)).leaf,
                      terminal(LexerToken(Token.Delimiter.`:`, 2100)).leaf,
                      terminal(LexerToken(Token.StringLiteral("get"), 2200)).leaf
                    ),
                    nonTerminal("WebhookSpecificSetting").node(
                      terminal(LexerToken(Token.Keyword.`queryString`, 2300)).leaf,
                      terminal(LexerToken(Token.Delimiter.`:`, 2400)).leaf,
                      terminal(LexerToken(Token.StringLiteral("b=18"), 2500)).leaf
                    )
                  )
                )
              )
            )
          )
        }) should be (true)
      }
    }

    "there is an error (invalid delimiter in namespace)" should {
      "fail" in {
        //arrange
        val tokens = Seq(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`:`, 13),
          LexerToken(Token.Identifier("create"), 14)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get should be (Seq(ParseError(13, "Unexpected: ':' (expecting: '#')")))
      }
    }

    "there is an error (no indent before 'description')" should {
      "fail" in {
        //arrange
        val tokens = Seq(
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
        result.toEither.left.get should be (Seq(ParseError(23, "Unexpected: 'description' (expecting: 'indent')")))
      }
    }

    "there is an error (got identifier instead of string literal in description)" should {
      "succeed" in {
        //arrange
        val tokens = Seq(
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
        result.toEither.left.get should be (Seq(ParseError(35, "Unexpected: 'com' (expecting: 'string literal')")))
      }
    }

    "there is an error (invalid indent before '@webhook')" should {
      "succeed" in {
        //arrange
        val tokens = Seq(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.StringLiteral("hello"), 35),

          LexerToken(Token.INDENT(2), 40),
          LexerToken(Token.Keyword.`@webhook`, 45)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get should be (Seq(ParseError(40, "Unexpected indent width: 2")))
      }
    }

    "there is an error (invalid keyword instead of mandatory 'uri' inside '@webhook')" should {
      "succeed" in {
        //arrange
        val tokens = Seq(
          LexerToken(Token.INDENT(0), 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14),

          LexerToken(Token.INDENT(3), 20),
          LexerToken(Token.Keyword.`description`, 23),
          LexerToken(Token.Delimiter.`:`, 26),
          LexerToken(Token.StringLiteral("hello"), 35),

          LexerToken(Token.INDENT(3), 40),
          LexerToken(Token.Keyword.`@webhook`, 45),

          LexerToken(Token.INDENT(5), 50),
          LexerToken(Token.Keyword.`method`, 55),
          LexerToken(Token.Delimiter.`:`, 56),
          LexerToken(Token.StringLiteral("/example"), 61)
        )

        //act
        val result = parser.parse(tokens)

        //assert
        result.toEither.left.get should be (Seq(ParseError(55, "Unexpected: 'method' (expecting: 'uri')")))
      }
    }
  }
}
