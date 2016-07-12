package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexicalAnalyzer.LexerToken
import com.ialekseev.bob.syntax.SyntaxAnalyzer._
import scalaz._
import Scalaz._

//Top-Down Predictive Parsing for LL1 grammar (Recursive Descent technique)
class LLSyntaxAnalyzer extends SyntaxAnalyzer with LLSyntaxAnalysisState {

  //NamespacePathPart ::= '.' identifier
  private def parseNamespacePathPart: Parsed[ParseTree] = rule("NamespacePathPart") {
    for {
      dot <- parse[Token.Delimiter.`.`.type]
      identifier <- parse[Token.Identifier]
    } yield Seq(dot, identifier)
  }

  //NamespacePathParts ::= {NamespacePathPart}
  private def parseNamespacePathParts: Parsed[Option[ParseTree]] = {
    repeat(parseNamespacePathPart, "NamespacePathParts")
  }

  //NamespacePath ::= identifier NamespacePathParts
  private def parseNamespacePath: Parsed[ParseTree] = rule("NamespacePath") {
    for {
      identifier <- parse[Token.Identifier]
      dotPath <- parseNamespacePathParts
    } yield identifier +: dotPath.toSeq
  }

  //Namespace ::= INDENT(0) 'namespace' NamespacePath # identifier
  private def parseNamespace: Parsed[ParseTree] = rule("Namespace") {
    for {
      _ <- parseIndent(0)
      namespaceKeyword <- parse[Token.Keyword.`namespace`.type]
      namespacePath <- parseNamespacePath
      pound <- parse[Token.Delimiter.`#`.type]
      identifier <- parse[Token.Identifier]
    } yield Seq(namespaceKeyword, namespacePath, pound, identifier)
  }

  //Description ::= INDENT(1) 'description' : stringLiteral
  private def parseDescription: Parsed[ParseTree] = rule("Description") {
    for {
      _ <- parseIndent(1)
      descriptionKeyword <- parse[Token.Keyword.`description`.type]
      colon <- parse[Token.Delimiter.`:`.type]
      stringLiteral <- parse[Token.StringLiteral]
    } yield Seq(descriptionKeyword, colon, stringLiteral)
  }

  //Constant ::= INDENT(1) variable : stringLiteral
  private def parseConstant: Parsed[ParseTree] = rule("Constant") {
    for {
      _ <- parseIndent(1)
      variable <- parse[Token.Variable]
      colon <- parse[Token.Delimiter.`:`.type]
      stringLiteral <- parse[Token.StringLiteral]
    } yield Seq(variable, colon, stringLiteral)
  }

  //Constants ::= {Constant}
  private def parseConstants: Parsed[Option[ParseTree]] = {
    repeat(parseConstant, "Constants")
  }

  //WebhookUriSetting ::= INDENT(2) 'uri' : stringLiteral
  private def parseWebhookUriSetting: Parsed[ParseTree] = rule("WebhookUriSetting") {
    for {
      _ <- parseIndent(2)
      keyword <- parse[Token.Keyword.`uri`.type]
      colon <- parse[Token.Delimiter.`:`.type]
      stringLiteral <- parse[Token.StringLiteral]
    } yield Seq(keyword, colon, stringLiteral)
  }

  /*WebhookSpecificSetting ::= INDENT(2) 'method' : stringLiteral |
                               INDENT(2) 'queryString' : dictionary*/
  private def parseWebhookSpecificSetting: Parsed[ParseTree] = {
    val methodT = for {
      indent <- parseIndent(2)
      method <- parse[Token.Keyword.`method`.type]
      colon <- parse[Token.Delimiter.`:`.type]
      stringLiteral <- parse[Token.StringLiteral]
    } yield Seq(method, colon, stringLiteral)

    val queryStringT = for {
      indent <- parseIndent(2)
      queryString <- parse[Token.Keyword.`queryString`.type]
      colon <- parse[Token.Delimiter.`:`.type]
      dictionary <- parse[Token.Dictionary]
    } yield Seq(queryString, colon, dictionary)

    rule("WebhookSpecificSetting") {
      methodT orElse queryStringT
    }
  }

  //WebhookSpecificSettings ::= {WebhookSpecificSetting}
  private def parseWebhookSpecificSettings: Parsed[Option[ParseTree]] = {
    repeat(parseWebhookSpecificSetting, "WebhookSpecificSettings")
  }

  /*WebhookSettings ::= WebhookUriSetting
                        {WebhookSpecificSetting}*/
  private def parseWebhookSettings: Parsed[ParseTree] = rule("WebhookSettings") {
    for {
      uriSetting <- parseWebhookUriSetting
      specificSettings <- parseWebhookSpecificSettings
    } yield uriSetting +: specificSettings.toSeq
  }

  /*Webhook ::= INDENT(1) '@webhook'
                          WebhookSettings*/
  private def parseWebhook: Parsed[ParseTree] = rule("Webhook") {
    for {
      _ <- parseIndent(1)
      webhookKeyword <- parse[Token.Keyword.`@webhook`.type]
      webhookSettings <- parseWebhookSettings
    } yield Seq(webhookKeyword, webhookSettings)
  }

  /*Rule ::= Description
             Constants
             Webhook*/
  private def parseRule: Parsed[ParseTree] = rule("Rule") {
    for {
      description <- parseDescription
      constants <- parseConstants
      webhook <- parseWebhook
    } yield Seq(description) |+| constants.toSeq |+| Seq(webhook)
  }

  /*TopStat ::= Namespace
                Rule*/
  private def parseTopStat: Parsed[ParseTree] = rule("TopStat") {
    for {
      namespace <- parseNamespace
      rule <- parseRule
    } yield Seq(namespace, rule)
  }

  def parse(tokens: Seq[LexerToken]): \/[Seq[ParseError], ParseTree] = {
    require(tokens.nonEmpty)

    parseTopStat.run(ParserStateInternal(tokens, 0, Map.empty))._2
  }
}