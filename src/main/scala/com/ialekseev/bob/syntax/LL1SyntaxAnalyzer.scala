package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexicalAnalyzer.LexerToken
import com.ialekseev.bob.syntax.SyntaxAnalyzer._
import scalaz._
import Scalaz._
import scala.reflect.ClassTag

//Top-Down Predictive Parsing for LL1 grammar (Recursive Descent technique)
class LL1SyntaxAnalyzer extends SyntaxAnalyzer with LL1SyntaxAnalysisState {

  //NamespacePathPart ::= '.' identifier
  private def parseNamespacePathPart: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      dot <- EitherT.eitherT(parseToken[Token.Delimiter.`.`.type].toTree)
      identifier <- EitherT.eitherT(parseToken[Token.Identifier].toTree)
    } yield Seq(dot, identifier)).run

    attachToNonTerminal(parsed, "NamespacePathPart")
  }

  //NamespacePathParts ::= {NamespacePathPart}
  private def parseNamespacePathParts: Parsed[Option[ParseTree]] = {
    repeat(parseNamespacePathPart, "NamespacePathParts")
  }

  //NamespacePath ::= identifier NamespacePathParts
  private def parseNamespacePath: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      identifier <- EitherT.eitherT(parseToken[Token.Identifier].toTree)
      dotPath <- EitherT.eitherT(parseNamespacePathParts)
    } yield identifier +: dotPath.toSeq).run

    attachToNonTerminal(parsed, "NamespacePath")
  }

  //Namespace ::= INDENT(0) 'namespace' NamespacePath # identifier
  private def parseNamespace: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      _ <- EitherT.eitherT(parseIndentToken(0).toTree)
      namespaceKeyword <- EitherT.eitherT(parseToken[Token.Keyword.`namespace`.type].toTree)
      namespacePath <- EitherT.eitherT(parseNamespacePath)
      pound <- EitherT.eitherT(parseToken[Token.Delimiter.`#`.type].toTree)
      identifier <- EitherT.eitherT(parseToken[Token.Identifier].toTree)
    } yield Seq(namespaceKeyword, namespacePath, pound, identifier)).run

    attachToNonTerminal(parsed, "Namespace")
  }

  //Description ::= INDENT(1) 'description' : stringLiteral
  private def parseDescription: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      _ <- EitherT.eitherT(parseIndentToken(1).toTree)
      descriptionKeyword <- EitherT.eitherT(parseToken[Token.Keyword.`description`.type].toTree)
      colon <- EitherT.eitherT(parseToken[Token.Delimiter.`:`.type].toTree)
      stringLiteral <- EitherT.eitherT(parseToken[Token.StringLiteral].toTree)
    } yield Seq(descriptionKeyword, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "Description")
  }

  //Constant ::= INDENT(1) variable : stringLiteral
  private def parseConstant: Parsed[ParseTree] = {

    var parsed: Parsed[Seq[ParseTree]] = (for {
      _ <- EitherT.eitherT(parseIndentToken(1).toTree)
      variable <- EitherT.eitherT(parseToken[Token.Variable].toTree)
      colon <- EitherT.eitherT(parseToken[Token.Delimiter.`:`.type].toTree)
      stringLiteral <- EitherT.eitherT(parseToken[Token.StringLiteral].toTree)
    } yield Seq(variable, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "Constant")
  }

  //Constants ::= {Constant}
  private def parseConstants: Parsed[Option[ParseTree]] = {
    repeat(parseConstant, "Constants")
  }

  //WebhookUriSetting ::= INDENT(2) 'uri' : stringLiteral
  private def parseWebhookUriSetting: Parsed[ParseTree] = {
    val parsed: Parsed[Seq[ParseTree]] = (for {
      _ <- EitherT.eitherT(parseIndentToken(2).toTree)
      keyword <- EitherT.eitherT(parseToken[Token.Keyword.`uri`.type].toTree)
      colon <- EitherT.eitherT(parseToken[Token.Delimiter.`:`.type].toTree)
      stringLiteral <- EitherT.eitherT(parseToken[Token.StringLiteral].toTree)
    } yield Seq(keyword, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "WebhookUriSetting")
  }

  /*WebhookSpecificSetting ::= INDENT(2) 'method' : stringLiteral |
                               INDENT(2) 'queryString' : stringLiteral*/
  private def parseWebhookSpecificSetting: Parsed[ParseTree] = {

    val keywordT = EitherT.eitherT(parseToken[Token.Keyword.`method`.type].toTree) orElse
                 EitherT.eitherT(parseToken[Token.Keyword.`queryString`.type].toTree)

    val parsed: Parsed[Seq[ParseTree]] = (for {
      _ <- EitherT.eitherT(parseIndentToken(2).toTree)
      keyword <- keywordT
      colon <- EitherT.eitherT(parseToken[Token.Delimiter.`:`.type].toTree)
      stringLiteral <- EitherT.eitherT(parseToken[Token.StringLiteral].toTree)
    } yield Seq(keyword, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "WebhookSpecificSetting")
  }

  //WebhookSpecificSettings ::= {WebhookSpecificSetting}
  private def parseWebhookSpecificSettings: Parsed[Option[ParseTree]] = {
    repeat(parseWebhookSpecificSetting, "WebhookSpecificSettings")
  }

  /*WebhookSettings ::= WebhookUriSetting
                        {WebhookSpecificSetting}*/
  private def parseWebhookSettings: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      uriSetting <- EitherT.eitherT(parseWebhookUriSetting)
      specificSettings <- EitherT.eitherT(parseWebhookSpecificSettings)
    } yield uriSetting +: specificSettings.toSeq).run

    attachToNonTerminal(parsed, "WebhookSettings")
  }

  /*Webhook ::= INDENT(1) '@webhook'
                          WebhookSettings*/
  private def parseWebhook: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      _ <- EitherT.eitherT(parseIndentToken(1).toTree)
      webhookKeyword <- EitherT.eitherT(parseToken[Token.Keyword.`@webhook`.type].toTree)
      webhookSettings <- EitherT.eitherT(parseWebhookSettings)
    } yield Seq(webhookKeyword, webhookSettings)).run

    attachToNonTerminal(parsed, "Webhook")
  }

  /*Rule ::= Description
             Constants
             Webhook*/
  private def parseRule: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      description <- EitherT.eitherT(parseDescription)
      constants <- EitherT.eitherT(parseConstants)
      webhook <- EitherT.eitherT(parseWebhook)
    } yield Seq(description) |+| constants.toSeq |+| Seq(webhook)).run

    attachToNonTerminal(parsed, "Rule")
  }

  /*TopStat ::= Namespace
                Rule*/
  private def parseTopStat: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      namespace <- EitherT.eitherT(parseNamespace)
      rule <- EitherT.eitherT(parseRule)
    } yield Seq(namespace, rule)).run

    attachToNonTerminal(parsed, "TopStat")
  }

  def parse(tokens: Seq[LexerToken]): \/[Seq[ParseError], ParseTree] = {
    require(tokens.nonEmpty)

    parseTopStat.run(ParserStateInternal(tokens, 0, Map.empty))._2
  }
}