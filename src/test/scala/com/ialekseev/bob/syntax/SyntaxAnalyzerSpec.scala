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
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Keyword.`namespace`, 0),
          LexerToken(Token.Identifier("com"), 10),
          LexerToken(Token.Delimiter.`#`, 13),
          LexerToken(Token.Identifier("create"), 14)
        )

        //act
        val result = parser.parse()

        //assert
        result.toEither.right.get should be(empty)
      }
    }
  }*/
}
