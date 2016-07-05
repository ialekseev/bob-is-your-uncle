package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexicalAnalyzer.LexerToken
import com.ialekseev.bob.syntax.SyntaxAnalyzer._
import com.ialekseev.bob.syntax.LL1SyntaxAnalysisState._
import scalaz._
import Scalaz._

//Top-Down Predictive Parsing for LL1 grammar (Recursive Descent technique)
class LL1SyntaxAnalyzer extends SyntaxAnalyzer with LL1SyntaxAnalysisState {

  //NamespacePathPart ::= '.' identifier
  private def parseNamespacePathPart: Parsed[ParseTree] = rule {
    for {
      dot <- EitherT.eitherT(parseToken[Token.Delimiter.`.`.type].toTree)
      identifier <- EitherT.eitherT(parseToken[Token.Identifier].toTree)
    } yield Seq(dot, identifier)
  }.attachToNonTerminal("NamespacePathPart")

  //NamespacePathParts ::= {NamespacePathPart}
  private def parseNamespacePathParts: Parsed[Option[ParseTree]] = {
    repeat(parseNamespacePathPart, "NamespacePathParts")
  }

  //NamespacePath ::= identifier NamespacePathParts
  private def parseNamespacePath: Parsed[ParseTree] = rule {
    for {
      identifier <- EitherT.eitherT(parseToken[Token.Identifier].toTree)
      dotPath <- EitherT.eitherT(parseNamespacePathParts)
    } yield identifier +: dotPath.toSeq
  }.attachToNonTerminal("NamespacePath")

  //Namespace ::= INDENT(0) 'namespace' NamespacePath # identifier
  private def parseNamespace: Parsed[ParseTree] = rule {
    for {
      _ <- EitherT.eitherT(parseIndentToken(0).toTree)
      namespaceKeyword <- EitherT.eitherT(parseToken[Token.Keyword.`namespace`.type].toTree)
      namespacePath <- EitherT.eitherT(parseNamespacePath)
      pound <- EitherT.eitherT(parseToken[Token.Delimiter.`#`.type].toTree)
      identifier <- EitherT.eitherT(parseToken[Token.Identifier].toTree)
    } yield Seq(namespaceKeyword, namespacePath, pound, identifier)
  }.attachToNonTerminal("Namespace")


  //Description ::= INDENT(1) 'description' : stringLiteral
  private def parseDescription: Parsed[ParseTree] = rule {
    for {
      _ <- EitherT.eitherT(parseIndentToken(1).toTree)
      descriptionKeyword <- EitherT.eitherT(parseToken[Token.Keyword.`description`.type].toTree)
      colon <- EitherT.eitherT(parseToken[Token.Delimiter.`:`.type].toTree)
      stringLiteral <- EitherT.eitherT(parseToken[Token.StringLiteral].toTree)
    } yield Seq(descriptionKeyword, colon, stringLiteral)
  }.attachToNonTerminal("Description")

  //Constant ::= INDENT(1) variable : stringLiteral
  private def parseConstant: Parsed[ParseTree] = rule {
    for {
      _ <- EitherT.eitherT(parseIndentToken(1).toTree)
      variable <- EitherT.eitherT(parseToken[Token.Variable].toTree)
      colon <- EitherT.eitherT(parseToken[Token.Delimiter.`:`.type].toTree)
      stringLiteral <- EitherT.eitherT(parseToken[Token.StringLiteral].toTree)
    } yield Seq(variable, colon, stringLiteral)
  }.attachToNonTerminal("Constant")

  //Constants ::= {Constant}
  private def parseConstants: Parsed[Option[ParseTree]] = {
    repeat(parseConstant, "Constants")
  }

  //WebhookUriSetting ::= INDENT(2) 'uri' : stringLiteral
  private def parseWebhookUriSetting: Parsed[ParseTree] = rule {
    for {
      _ <- EitherT.eitherT(parseIndentToken(2).toTree)
      keyword <- EitherT.eitherT(parseToken[Token.Keyword.`uri`.type].toTree)
      colon <- EitherT.eitherT(parseToken[Token.Delimiter.`:`.type].toTree)
      stringLiteral <- EitherT.eitherT(parseToken[Token.StringLiteral].toTree)
    } yield Seq(keyword, colon, stringLiteral)
  }.attachToNonTerminal("WebhookUriSetting")


  /*WebhookSpecificSetting ::= INDENT(2) 'method' : stringLiteral |
                               INDENT(2) 'queryString' : stringLiteral*/
  private def parseWebhookSpecificSetting: Parsed[ParseTree] = {

    val keywordT = EitherT.eitherT(parseToken[Token.Keyword.`method`.type].toTree) orElse
                 EitherT.eitherT(parseToken[Token.Keyword.`queryString`.type].toTree)
    rule {
      for {
        _ <- EitherT.eitherT(parseIndentToken(2).toTree)
        keyword <- keywordT
        colon <- EitherT.eitherT(parseToken[Token.Delimiter.`:`.type].toTree)
        stringLiteral <- EitherT.eitherT(parseToken[Token.StringLiteral].toTree)
      } yield Seq(keyword, colon, stringLiteral)
    }.attachToNonTerminal("WebhookSpecificSetting")
  }

  //WebhookSpecificSettings ::= {WebhookSpecificSetting}
  private def parseWebhookSpecificSettings: Parsed[Option[ParseTree]] = {
    repeat(parseWebhookSpecificSetting, "WebhookSpecificSettings")
  }

  /*WebhookSettings ::= WebhookUriSetting
                        {WebhookSpecificSetting}*/
  private def parseWebhookSettings: Parsed[ParseTree] = rule {
    for {
      uriSetting <- EitherT.eitherT(parseWebhookUriSetting)
      specificSettings <- EitherT.eitherT(parseWebhookSpecificSettings)
    } yield uriSetting +: specificSettings.toSeq
  }.attachToNonTerminal("WebhookSettings")

  /*Webhook ::= INDENT(1) '@webhook'
                          WebhookSettings*/
  private def parseWebhook: Parsed[ParseTree] = rule {
    for {
      _ <- EitherT.eitherT(parseIndentToken(1).toTree)
      webhookKeyword <- EitherT.eitherT(parseToken[Token.Keyword.`@webhook`.type].toTree)
      webhookSettings <- EitherT.eitherT(parseWebhookSettings)
    } yield Seq(webhookKeyword, webhookSettings)
  }.attachToNonTerminal("Webhook")

  /*Rule ::= Description
             Constants
             Webhook*/
  private def parseRule: Parsed[ParseTree] = rule {
    for {
      description <- EitherT.eitherT(parseDescription)
      constants <- EitherT.eitherT(parseConstants)
      webhook <- EitherT.eitherT(parseWebhook)
    } yield Seq(description) |+| constants.toSeq |+| Seq(webhook)
  }.attachToNonTerminal("Rule")

  /*TopStat ::= Namespace
                Rule*/
  private def parseTopStat: Parsed[ParseTree] = rule {
    for {
      namespace <- EitherT.eitherT(parseNamespace)
      rule <- EitherT.eitherT(parseRule)
    } yield Seq(namespace, rule)
  }.attachToNonTerminal("TopStat")

  def parse(tokens: Seq[LexerToken]): \/[Seq[ParseError], ParseTree] = {
    require(tokens.nonEmpty)

    parseTopStat.run(ParserStateInternal(tokens, 0, Map.empty))._2
  }
}