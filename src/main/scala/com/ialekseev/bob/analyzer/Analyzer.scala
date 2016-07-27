package com.ialekseev.bob.analyzer

import com.ialekseev.bob.analyzer.lexical.{AdHocLexicalAnalyzer, LexicalAnalyzer}
import com.ialekseev.bob.analyzer.syntax.SyntaxAnalyzer._
import com.ialekseev.bob.analyzer.syntax.{AdHocSyntaxAnalyzer, SyntaxAnalyzer}
import org.json4s.JsonAST.{JValue}
import HttpMethod.HttpMethod
import scalaz._
import Scalaz._

trait Analyzer {
  val lexicalAnalyzer: LexicalAnalyzer
  val syntaxAnalyzer: SyntaxAnalyzer

  def analyze(source: String): AnalysisFailed \/ AnalysisResult = {
    require(!source.isEmpty)

    parse(source) match {
      case \/-(parseTree) => mapTreeToAnalysisResult(parseTree)
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

  protected def mapTreeToAnalysisResult(parseTree: ParseTree): SemanticAnalysisFailed \/ AnalysisResult = {
    val id: PartialFunction[ParseTreeNode, String] = { case Terminal(LexerToken(Token.Identifier(s), _)) => s }
    val stringLiteral: PartialFunction[ParseTreeNode, String] = { case Terminal(LexerToken(Token.Type.StringLiteral(s), _)) => s }

    def extractNamespace: ValidationNel[SemanticError, Namespace]  = {
      val namespace = parseTree.loc.find(_.getLabel == nonTerminal("Namespace"))
      val namespacePath = namespace >>= (_.find(_.getLabel == nonTerminal("NamespacePath")))

      val namespacePathMain = (namespacePath >>= (_.firstChild)).map(_.getLabel).collect(id).getOrElse(sys.error("Missing namespace main part"))
      val namespacePathOptional = (namespacePath >>= (_.find(_.getLabel == nonTerminal("NamespacePathParts")).map(parts => {
        parts.tree.flatten.collect(id)
      }))).getOrElse(Stream.empty)

      val path = namespacePathOptional.fold(namespacePathMain)(_ + "." + _)
      val name = (namespace >>= (_.lastChild)).map(_.getLabel).collect(id).getOrElse(sys.error("Missing namespace name"))
      Namespace(path, name).successNel
    }

    def extractDescription: ValidationNel[SemanticError, String] = {
      (parseTree.loc.find(_.getLabel == nonTerminal("Description")) >>= (_.lastChild)).map(_.getLabel).collect(stringLiteral).getOrElse(sys.error("Missing description")).successNel
    }

    def extractWebhook: ValidationNel[SemanticError, Webhook] = {
      val settings = parseTree.loc.find(_.getLabel == nonTerminal("WebhookSettings"))
      val uri = (settings >>= (_.find(_.getLabel == nonTerminal("WebhookUriSetting"))) >>= (_.lastChild)).map(_.getLabel).collect(stringLiteral).getOrElse(sys.error("Missing webhook uri"))
      val specificSettings = ((settings >>= (_.find(_.getLabel == nonTerminal("WebhookSpecificSettings")))).map(_.tree.subForest) >>=
                              (_.map(s => (s.loc.firstChild |@| s.loc.lastChild)((_, _))).sequence)).getOrElse(Stream.empty)

      def extractMethod: ValidationNel[SemanticError, HttpMethod]  = {
        val method = specificSettings.map(s => (s._1.getLabel, s._2.getLabel)).collect { case (Terminal(LexerToken(Token.Keyword.`method`, _)), Terminal(LexerToken(Token.Type.StringLiteral(m), off))) => (m, off) }.headOption
        method match {
          case Some((m, offset)) => HttpMethod.values.find(_.toString.toUpperCase == m.toUpperCase).map(_.successNel[SemanticError]).getOrElse(SemanticError(offset, "Unexpected Http method").failureNel[HttpMethod])
          case _ => HttpMethod.GET.successNel
        }
      }

      val headers = specificSettings.map(s => (s._1.getLabel, s._2.getLabel)).collect { case (Terminal(LexerToken(Token.Keyword.`headers`, _)), Terminal(LexerToken(Token.Type.Dictionary(_, h), _))) => h }.headOption.getOrElse(Map.empty)
      val queryString = specificSettings.map(s => (s._1.getLabel, s._2.getLabel)).collect { case (Terminal(LexerToken(Token.Keyword.`queryString`, _)), Terminal(LexerToken(Token.Type.Dictionary(_, q), _))) => q }.headOption.getOrElse(Map.empty)
      val body: Option[Body] = ((specificSettings.map(s => (s._1.getLabel, s._2)).collect { case (Terminal(LexerToken(Token.Keyword.`body`, _)), b) => b }.headOption) >>=
        (_.find(_.getLabel == nonTerminal("WebhookSpecificSettingBodyType"))) >>= (_.firstChild.map(_.getLabel))) collect {
          case Terminal(LexerToken(Token.Type.StringLiteral(s), _)) => StringLiteralBody(s)
          case Terminal(LexerToken(Token.Type.Dictionary(_, d), _)) => DictionaryBody(d)
          case Terminal(LexerToken(Token.Type.Json(_, j), _)) => JsonBody(j)
      }

      extractMethod.map(method => Webhook(uri, method, headers, queryString, body))
    }

    def extractCode: ValidationNel[SemanticError, Code] = {
      (parseTree.loc.find(_.getLabel == nonTerminal("Process")) >>= (_.find(_.getLabel == nonTerminal("Block"))) >>= (_.firstChild.map(_.getLabel))).collect {
        case Terminal(LexerToken(Token.Block.`<scala>`(s), _)) => ScalaCode(s)
      }.getOrElse(sys.error("Missing code block")).successNel
    }

    (extractNamespace |@| extractDescription |@| extractWebhook |@| extractCode)(AnalysisResult(_,_,_,_)).disjunction.leftMap(e => SemanticAnalysisFailed(e.toVector))
  }
}

object Analyzer {
  def apply() = new Analyzer {
    val lexicalAnalyzer = new AdHocLexicalAnalyzer
    val syntaxAnalyzer = new AdHocSyntaxAnalyzer
  }
}

case class AnalysisResult(namespace: Namespace, description: String, webhook: Webhook, code: Code)

case class Namespace(path: String, name: String)
case class Webhook(uri: String, method: HttpMethod, headers: Map[String, String], queryString: Map[String, String], body: Option[Body])

object HttpMethod extends Enumeration {
  type HttpMethod = Value
  val GET, POST, PUT, DELETE = Value
}

sealed trait Body
case class StringLiteralBody(text: String) extends Body
case class DictionaryBody(dic: Map[String, String]) extends Body
case class JsonBody(j: JValue) extends Body

sealed trait Code
case class ScalaCode(source: String) extends Code