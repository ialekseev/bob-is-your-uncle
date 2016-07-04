package com.ialekseev.bob.syntax

import com.ialekseev.bob.lexical.LexicalAnalyzer.LexerToken
import com.ialekseev.bob.syntax.SyntaxAnalyzer._
import com.ialekseev.bob.{Token, BaseSpec}
import scalaz._
import Scalaz._

class SyntaxAnalyzerSpec extends BaseSpec {
  val parser = new LL1SyntaxAnalyzer()

  "Doing syntax analysis" when {

    "there is a minimal valid set of lexer tokens" should {
      "succeed in building parse tree" in {
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
  }
}
