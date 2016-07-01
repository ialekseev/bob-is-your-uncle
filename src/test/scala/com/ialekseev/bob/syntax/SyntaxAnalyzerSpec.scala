package com.ialekseev.bob.syntax

import com.ialekseev.bob.lexical.LexicalAnalyzer.LexerToken
import com.ialekseev.bob.{Token, BaseSpec}

class SyntaxAnalyzerSpec extends BaseSpec {
  val parser = new LL1SyntaxAnalyzer()

  /*"Doing syntax analysis" when {

    "there are minimal valid set of lexer tokens" should {
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
        )

        //act
        val result = parser.parse()

        //assert
        result.toEither.right.get should be(empty)
      }
    }
  }*/
}
