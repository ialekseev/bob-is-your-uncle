package com.ialekseev.bob.analyzer

import com.ialekseev.bob.analyzer.lexical.{AdHocLexicalAnalyzer, LexicalAnalyzer}
import com.ialekseev.bob.analyzer.syntax.LLSyntaxAnalyzer.ParseTree
import com.ialekseev.bob.analyzer.syntax.{AdHocSyntaxAnalyzer, LLSyntaxAnalyzer}
import org.json4s.JsonAST.JValue
import scalaz._
import Scalaz._

trait Analyzer {
  val lexicalAnalyzer: LexicalAnalyzer
  val syntaxAnalyzer: LLSyntaxAnalyzer

  protected def parse(source: String): AnalysisFailed \/ ParseTree = {
    lexicalAnalyzer.tokenize(source) match {
      case \/-(tokens) => syntaxAnalyzer.parse(tokens)
      case lexFailed@ -\/(_) => lexFailed
    }
  }

  protected def mapTreeToAnalysisResult(parseTree: ParseTree): AnalysisResult = {
    ???
  }

  def analyze(source: String): AnalysisFailed \/ AnalysisResult = {
    require(!source.isEmpty)

    parse(source) match {
      case \/-(parseTree) => mapTreeToAnalysisResult(parseTree).right
      case syntaxFailed@ -\/(_) => syntaxFailed
    }
  }
}

object Analyzer {
  def apply() = new Analyzer {
    val lexicalAnalyzer = new AdHocLexicalAnalyzer
    val syntaxAnalyzer = new AdHocSyntaxAnalyzer
  }
}

case class AnalysisResult(namespace: Namespace, description: String, constants: Map[String, String], webhook: Webhook, process: Code)

case class Namespace(path: String, name: String)
case class Webhook(uri: String, method: Option[String], headers: Map[String, String], queryString: Map[String, String], body: Option[Body])

sealed trait Body
case class StringLiteralBody(text: String) extends Body
case class DictionaryBody(dic: Map[String, String]) extends Body
case class JsonBody(j: JValue) extends Body

sealed trait Code
case class ScalaCode(code: String) extends Code