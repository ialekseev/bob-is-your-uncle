package com.ialekseev.bob.analyzer

import com.ialekseev.bob._
import com.ialekseev.bob.analyzer.Analyzer._
import com.ialekseev.bob.analyzer.lexical.{AdHocLexicalAnalyzer, LexicalAnalyzer}
import com.ialekseev.bob.analyzer.syntax.SyntaxAnalyzer._
import com.ialekseev.bob.analyzer.syntax.{AdHocSyntaxAnalyzer, SyntaxAnalyzer}
import org.json4s.JsonAST.{JValue}
import scalaz._
import Scalaz._

trait Analyzer {
  val lexicalAnalyzer: LexicalAnalyzer
  val syntaxAnalyzer: SyntaxAnalyzer

  def analyze(source: String): StageFailed \/ AnalysisResult = {
    if (source.nonEmpty) {
      parse(source) match {
        case \/-(parseTree) => mapTreeToAnalysisResult(parseTree)
        case syntaxFailed@ -\/(_) => syntaxFailed
      }
    } else SemanticAnalysisFailed(Seq(SemanticError(0, 0, "Empty source is not expected!"))).left
  }

  protected def parse(source: String): StageFailed \/ ParseTree = {
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

      val namespacePathMain = (namespacePath >>= (_.firstChild)).map(_.getLabel).map(id).get
      val namespacePathOptional = (namespacePath >>= (_.find(_.getLabel == nonTerminal("NamespacePathParts")).map(parts => {
        parts.tree.flatten.collect(id)
      }))).getOrElse(Stream.empty)

      val path = namespacePathOptional.fold(namespacePathMain)(_ + "." + _)
      val name = (namespace >>= (_.lastChild)).map(_.getLabel).map(id).get
      Namespace(path, name).successNel
    }

    def extractDescription: ValidationNel[SemanticError, String] = {
      (parseTree.loc.find(_.getLabel == nonTerminal("Description")) >>= (_.lastChild)).map(_.getLabel).map(stringLiteral).get.successNel
    }

    def extractConstants:  ValidationNel[SemanticError, Seq[(String, String)]] = {
      parseTree.loc.find(_.getLabel == nonTerminal("Constants")).map(_.tree.subForest).getOrElse(Stream.empty).
        map(c => (c.loc.firstChild.map(_.getLabel).get, c.loc.lastChild.map(_.getLabel).get)).map {
        case (Terminal(LexerToken(Token.Variable(name), _)), Terminal(LexerToken(Token.Type.StringLiteral(value), _))) => (name, value)
        case _ => sys.error("Invalid constant")
      }.toSeq.successNel
    }

    def extractWebhook: ValidationNel[SemanticError, Webhook] = {
      val webhook = parseTree.loc.find(_.getLabel == nonTerminal("Webhook"))
      val settings = ((webhook >>= (_.find(_.getLabel == nonTerminal("WebhookSettings")))).map(_.tree.subForest) >>=
                              (_.map(s => (s.loc.firstChild |@| s.loc.lastChild)((_, _))).sequence)).getOrElse(Stream.empty)

      def extractMethod: ValidationNel[SemanticError, HttpMethod.Value]  = {
        val method = settings.map(s => (s._1.getLabel, s._2.getLabel)).collect { case (Terminal(LexerToken(Token.Keyword.`method`, _)), Terminal(LexerToken(Token.Type.StringLiteral(m), off))) => (m, off) }.headOption
        method match {
          case Some((m, offset)) => HttpMethod.values.find(_.toString.toUpperCase == m.toUpperCase).map(_.successNel[SemanticError]).getOrElse(SemanticError(offset + 1, offset + m.length, "Unexpected Http method").failureNel[HttpMethod.Value])
          case _ => HttpMethod.GET.successNel
        }
      }

      val uri = settings.map(s => (s._1.getLabel, s._2.getLabel)).collect { case (Terminal(LexerToken(Token.Keyword.`uri`, _)), Terminal(LexerToken(Token.Type.StringLiteral(u), _))) => u }.headOption.filter(!_.isEmpty)
      val headers = settings.map(s => (s._1.getLabel, s._2.getLabel)).collect { case (Terminal(LexerToken(Token.Keyword.`headers`, _)), Terminal(LexerToken(Token.Type.Dictionary(_, h), _))) => h }.headOption.getOrElse(Map.empty)
      val queryString = settings.map(s => (s._1.getLabel, s._2.getLabel)).collect { case (Terminal(LexerToken(Token.Keyword.`queryString`, _)), Terminal(LexerToken(Token.Type.Dictionary(_, q), _))) => q }.headOption.getOrElse(Map.empty)
      val body: Option[Body] = ((settings.map(s => (s._1.getLabel, s._2)).collect { case (Terminal(LexerToken(Token.Keyword.`body`, _)), b) => b }.headOption) >>=
        (_.find(_.getLabel == nonTerminal("WebhookSettingBodyType"))) >>= (_.firstChild.map(_.getLabel))) collect {
          case Terminal(LexerToken(Token.Type.StringLiteral(s), _)) => StringLiteralBody(s)
          case Terminal(LexerToken(Token.Type.Dictionary(_, d), _)) => DictionaryBody(d)
          case Terminal(LexerToken(Token.Type.Json(_, j), _)) => JsonBody(j)
      }
      extractMethod.map(method => Webhook(HttpRequest(uri, method, headers, queryString, body)))
    }

    def extractCode: ValidationNel[SemanticError, Code] = {
      (parseTree.loc.find(_.getLabel == nonTerminal("Process")) >>= (_.find(_.getLabel == nonTerminal("Block"))) >>= (_.firstChild.map(_.getLabel))).collect {
        case Terminal(LexerToken(Token.Block.`<scala>`(s), _)) => ScalaCode(s)
      }.get.successNel
    }

    (extractNamespace |@| extractDescription |@| extractConstants |@| extractWebhook |@| extractCode)(AnalysisResult(_,_,_,_,_)).disjunction.leftMap(e => SemanticAnalysisFailed(e.toVector))
  }
}

object Analyzer {
  case class AnalysisResult(namespace: Namespace, description: String, constants: Seq[(String, String)], webhook: Webhook, code: Code)
  case class Namespace(path: String, name: String)
  case class Webhook(req: HttpRequest)

  sealed trait Code
  case class ScalaCode(c: String) extends Code
}

object DefaultAnalyzer extends Analyzer {
  val lexicalAnalyzer = new AdHocLexicalAnalyzer
  val syntaxAnalyzer = new AdHocSyntaxAnalyzer
}