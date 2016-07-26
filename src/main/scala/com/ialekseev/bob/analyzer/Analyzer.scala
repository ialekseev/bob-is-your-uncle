package com.ialekseev.bob.analyzer

import com.ialekseev.bob.analyzer.lexical.{AdHocLexicalAnalyzer, LexicalAnalyzer}
import com.ialekseev.bob.analyzer.syntax.LLSyntaxAnalyzer._
import com.ialekseev.bob.analyzer.syntax.{AdHocSyntaxAnalyzer, LLSyntaxAnalyzer}
import org.json4s.JsonAST.JValue
import scalaz._
import Scalaz._

trait Analyzer {
  val lexicalAnalyzer: LexicalAnalyzer
  val syntaxAnalyzer: LLSyntaxAnalyzer

  def analyze(source: String): AnalysisFailed \/ AnalysisResult = {
    require(!source.isEmpty)

    parse(source) match {
      case \/-(parseTree) => mapTreeToAnalysisResult(parseTree).right
      case syntaxFailed@ -\/(_) => syntaxFailed
    }
  }

  protected def parse(source: String): AnalysisFailed \/ ParseTree = {
    require(!source.isEmpty)

    lexicalAnalyzer.tokenize(source) match {
      case \/-(tokens) => syntaxAnalyzer.parse(tokens)
      case lexFailed@ -\/(_) => lexFailed
    }
  }

  protected def mapTreeToAnalysisResult(parseTree: ParseTree): AnalysisResult = {
    val id: PartialFunction[ParseTreeNode, String] = { case Terminal(LexerToken(Token.Identifier(s), _)) => s }
    val stringLiteral: PartialFunction[ParseTreeNode, String] = { case Terminal(LexerToken(Token.Type.StringLiteral(s), _)) => s }

    def extractNamespace: Namespace = {
      val namespace = parseTree.loc.find(_.getLabel == nonTerminal("Namespace"))
      val namespacePath = namespace >>= (_.find(_.getLabel == nonTerminal("NamespacePath")))

      val namespacePathMain = (namespacePath >>= (_.firstChild)).map(_.getLabel).collect(id).getOrElse(sys.error("Missing namespace main part"))
      val namespacePathOptional = (namespacePath >>= (_.find(_.getLabel == nonTerminal("NamespacePathParts")).map(parts => {
        parts.tree.flatten.collect(id)
      }))).getOrElse(Stream.empty)

      val path = namespacePathOptional.fold(namespacePathMain)(_ + "." + _)
      val name = (namespace >>= (_.lastChild)).map(_.getLabel).collect(id).getOrElse(sys.error("Missing namespace name"))
      Namespace(path, name)
    }

    def extractDescription: String = {
      (parseTree.loc.find(_.getLabel == nonTerminal("Description")) >>= (_.lastChild)).map(_.getLabel).collect(stringLiteral).getOrElse(sys.error("Missing description"))
    }

    def extractWebhook: Webhook = {
      ???
    }

    val namespace = extractNamespace
    val description = extractDescription

    //todo: complete
    AnalysisResult(namespace, description, null, null, null)
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