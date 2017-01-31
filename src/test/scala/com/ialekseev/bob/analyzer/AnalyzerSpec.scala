package com.ialekseev.bob.analyzer

import com.ialekseev.bob.Models._
import com.ialekseev.bob.BaseSpec
import com.ialekseev.bob.analyzer.Analyzer._
import com.ialekseev.bob.analyzer.lexical.LexicalAnalyzer
import com.ialekseev.bob.analyzer.syntax.SyntaxAnalyzer
import com.ialekseev.bob.analyzer.syntax.SyntaxAnalyzer._
import org.json4s.JsonAST.{JObject, JString}
import org.mockito.Mockito
import org.mockito.Mockito._
import scalaz.Scalaz._
import scalaz.\/

class AnalyzerSpec extends BaseSpec {
  val lexer = mock[LexicalAnalyzer]
  val parser = mock[SyntaxAnalyzer]
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
        Mockito.when(parser.parse(Seq(LexerToken(Token.Keyword.`description`, 10)))).thenReturn(SyntaxAnalysisFailed(Seq(SyntaxError(10, 10, 0, "Bad!"))).left)

        //act
        val result = analyzer.analyze("source")

        //assert
        result should be (SyntaxAnalysisFailed(Seq(SyntaxError(10, 10, 0, "Bad!"))).left)
      }
    }

    "there is a semantic-error (invalid http method)" should {
      "fail with semantic error" in {
        //arrange
        val analyzer = new Analyzer {
          val lexicalAnalyzer = null
          val syntaxAnalyzer = null

          override def parse(source: String): StageFailed \/ ParseTree = {
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
                    nonTerminal("WebhookSetting").node(
                      terminal(LexerToken(Token.Keyword.`uri`, 900)).leaf,
                      terminal(LexerToken(Token.Delimiter.`:`, 1000)).leaf,
                      terminal(LexerToken(Token.Type.StringLiteral("/example"), 1100)).leaf
                    ),
                    nonTerminal("WebhookSetting").node(
                      terminal(LexerToken(Token.Keyword.`method`, 1200)).leaf,
                      terminal(LexerToken(Token.Delimiter.`:`, 1300)).leaf,
                      terminal(LexerToken(Token.Type.StringLiteral("GUT"), 1400)).leaf
                    )
                  )
                ),
                nonTerminal("Process").node(
                  terminal(LexerToken(Token.Keyword.`@process`, 1500)).leaf,
                  nonTerminal("Block").node(
                    terminal(LexerToken(Token.Block.`<scala>`("val a = 1"), 1600)).leaf
                  )
                )
              )
            ).right
          }
        }

        //act
        val result = analyzer.analyze("source")

        //assert
        result should be (SemanticAnalysisFailed(Seq(SemanticError(1401, 1403, "Unexpected Http method"))).left)
      }
    }

    "lexer & parser have succeeded with a minimal parse tree" should {
      "map the resulting parse tree to the analysis result" in {
        //arrange
        val analyzer = new Analyzer {
          val lexicalAnalyzer = null
          val syntaxAnalyzer = null

          override def parse(source: String): StageFailed \/ ParseTree = {
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
                  terminal(LexerToken(Token.Keyword.`@webhook`, 800)).leaf
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
        result should be (AnalysisResult(Namespace("com", "create"), "hello", Seq.empty, Webhook(HttpRequest(none, HttpMethod.GET, Map.empty, Map.empty, none)), ScalaCode("val a = 1")).right)
      }
    }

    "lexer & parser have succeeded with a maximum parse tree" should {
      def testCase(uri: String, resultUri: Option[String]) = {
        //arrange
        val analyzer = new Analyzer {
          val lexicalAnalyzer = null
          val syntaxAnalyzer = null

          override def parse(source: String): StageFailed \/ ParseTree = {
            source should be ("source")

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
                      terminal(LexerToken(Token.Type.StringLiteral(uri), 1900)).leaf
                    ),
                    nonTerminal("WebhookSetting").node(
                      terminal(LexerToken(Token.Keyword.`method`, 2000)).leaf,
                      terminal(LexerToken(Token.Delimiter.`:`, 2100)).leaf,
                      terminal(LexerToken(Token.Type.StringLiteral("post"), 2200)).leaf
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
                nonTerminal("Process").node(
                  terminal(LexerToken(Token.Keyword.`@process`, 3200)).leaf,
                  nonTerminal("Block").node(
                    terminal(LexerToken(Token.Block.`<scala>`("val a = 1"), 3300)).leaf
                  )
                )
              )
            ).right
          }
        }

        //act
        val result = analyzer.analyze("source")

        //assert
        result should be (AnalysisResult(Namespace("com.ialekseev", "create"), "hello", Seq("var1" -> "alice", "var2" -> "wonderland"), Webhook(HttpRequest(resultUri, HttpMethod.POST, Map("h1"->"a"), Map("b"->"18"), some(JsonBody(JObject("c"-> JString("19")))))), ScalaCode("val a = 1")).right)
      }

      "map the resulting parse tree to the analysis result (with uri)" in {
        testCase("/example", some("/example"))
      }

      "map the resulting parse tree to the analysis result (without uri, because it's empty)" in {
        testCase("", none)
      }
    }
  }

  override def beforeEach() = {
    reset(lexer)
    reset(parser)
  }
}
