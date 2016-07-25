package com.ialekseev.bob.analyzer

import com.ialekseev.bob.BaseSpec
import com.ialekseev.bob.analyzer.lexical.{LexicalAnalyzer}
import com.ialekseev.bob.analyzer.syntax.LLSyntaxAnalyzer._
import com.ialekseev.bob.analyzer.syntax.{LLSyntaxAnalyzer}
import org.mockito.Mockito
import org.mockito.Mockito._
import scalaz.Scalaz._
import scalaz.\/

class AnalyzerSpec extends BaseSpec {
  val lexer = mock[LexicalAnalyzer]
  val parser = mock[LLSyntaxAnalyzer]
  val analyzer = new Analyzer {
    val lexicalAnalyzer = lexer
    val syntaxAnalyzer = parser
  }

  "Analyzing" when {

    "lexer returns errors" should {
      "fail with lexical error" in {
        //arrange
        Mockito.when(lexer.tokenize("source")).thenReturn(LexicalAnalysisFailed(Seq(LexicalError(10, 20), LexicalError(30, 40))).left)

        //act
        val result = analyzer.analyze("source")

        //assert
        result should be (LexicalAnalysisFailed(Seq(LexicalError(10, 20), LexicalError(30, 40))).left)
      }
    }

    "parser returns errors" should {
      "fail with syntax error" in {
        //arrange
        Mockito.when(lexer.tokenize("source")).thenReturn(Seq(LexerToken(Token.Keyword.`description`, 10)).right)
        Mockito.when(parser.parse(Seq(LexerToken(Token.Keyword.`description`, 10)))).thenReturn(SyntaxAnalysisFailed(Seq(SyntaxError(10, 0, "Bad!"))).left)

        //act
        val result = analyzer.analyze("source")

        //assert
        result should be (SyntaxAnalysisFailed(Seq(SyntaxError(10, 0, "Bad!"))).left)
      }
    }

    "lexer & parser have succeeded with a minimal parse tree" should {
      "map the resulting parse tree to the analysis result" in {
        //arrange
        val analyzer = new Analyzer {
          val lexicalAnalyzer = null
          val syntaxAnalyzer = null

          override def parse(source: String): AnalysisFailed \/ ParseTree = {
            source should be ("source")

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
                  terminal(LexerToken(Token.Keyword.`@webhook`, 800)).leaf,
                  nonTerminal("WebhookSettings").node(
                    nonTerminal("WebhookUriSetting").node(
                      terminal(LexerToken(Token.Keyword.`uri`, 900)).leaf,
                      terminal(LexerToken(Token.Delimiter.`:`, 1000)).leaf,
                      terminal(LexerToken(Token.Type.StringLiteral("/example"), 1100)).leaf
                    )
                  )
                ),
                nonTerminal("Process").node(
                  terminal(LexerToken(Token.Keyword.`@process`, 1200)).leaf,
                  nonTerminal("Block").node(
                    terminal(LexerToken(Token.Block.`<scala>`("val a = 1"), 1300)).leaf
                  )
                )
              )
            ).right
          }
        }

        //act
        val result = analyzer.analyze("source")

        //assert
        result should be (AnalysisResult(Namespace("com", "create"), "hello", Map.empty, Webhook("/example", none, Map.empty, Map.empty, none), ScalaCode("val a = 1")).right)
      }
    }
  }

  override def beforeEach() = {
    reset(lexer)
    reset(parser)
  }
}
