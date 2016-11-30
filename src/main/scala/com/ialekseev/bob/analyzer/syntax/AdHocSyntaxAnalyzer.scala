package com.ialekseev.bob.analyzer.syntax

import com.ialekseev.bob.{SyntaxError, SyntaxAnalysisFailed}
import com.ialekseev.bob.analyzer.{LexerToken, Token}
import com.ialekseev.bob.analyzer.syntax.SyntaxAnalyzer._
import scalaz._
import Scalaz._

//Top-Down Predictive Parsing for LL grammar (Recursive Descent technique)
class AdHocSyntaxAnalyzer extends SyntaxAnalyzer with SyntaxAnalysisState {

  //NamespacePathPart ::= '.' identifier
  private def parseNamespacePathPart: Parsed[ParseTree] = rule("NamespacePathPart") {
    for {
      dot <- parse[Token.Delimiter.`.`.type]
      identifier <- parse[Token.Identifier]
    } yield Seq(dot, identifier)
  }

  //NamespacePathParts ::= {NamespacePathPart}
  private def parseNamespacePathParts: Parsed[Option[ParseTree]] = {
    repeat("NamespacePathParts")(parseNamespacePathPart)
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
      _ <- parse[Token.INDENT](0)
      namespaceKeyword <- parse[Token.Keyword.`namespace`.type]
      namespacePath <- parseNamespacePath
      pound <- parse[Token.Delimiter.`#`.type]
      identifier <- parse[Token.Identifier]
    } yield Seq(namespaceKeyword, namespacePath, pound, identifier)
  }

  //Description ::= INDENT(1) 'description' : stringLiteral
  private def parseDescription: Parsed[ParseTree] = rule("Description") {
    for {
      _ <- parse[Token.INDENT](1)
      descriptionKeyword <- parse[Token.Keyword.`description`.type]
      colon <- parse[Token.Delimiter.`:`.type]
      stringLiteral <- parse[Token.Type.StringLiteral]
    } yield Seq(descriptionKeyword, colon, stringLiteral)
  }

  //Constant ::= INDENT(1) variable : stringLiteral
  private def parseConstant: Parsed[ParseTree] = rule("Constant") {
    for {
      _ <- parse[Token.INDENT](1)
      variable <- parse[Token.Variable]
      colon <- parse[Token.Delimiter.`:`.type]
      stringLiteral <- parse[Token.Type.StringLiteral]
    } yield Seq(variable, colon, stringLiteral)
  }

  //Constants ::= {Constant}
  private def parseConstants: Parsed[Option[ParseTree]] = {
    repeat("Constants")(parseConstant)
  }

  //WebhookSettingBodyType ::= 'stringLiteral' | 'dictionary' | 'json'
  private def parseWebhookSettingBodyType: Parsed[ParseTree] = {
    or("WebhookSettingBodyType")("Expecting some valid Body type here")(
      for(stringLiteral <- parse[Token.Type.StringLiteral]) yield Seq(stringLiteral),
      for(dictionary <- parse[Token.Type.Dictionary]) yield Seq(dictionary),
      for(json <- parse[Token.Type.Json]) yield Seq(json)
    )
  }

  /*WebhookSetting ::= INDENT(2) 'uri' : stringLiteral |
                       INDENT(2) 'method' : stringLiteral |
                       INDENT(2) 'headers' : dictionary |
                       INDENT(2) 'queryString' : dictionary |
                       INDENT(2) 'body': WebhookSettingBodyType*/
  private def parseWebhookSetting: Parsed[ParseTree] = {
    or("WebhookSetting")("Expecting some valid Webhook setting here")(
      for {
        _ <- parse[Token.INDENT](2)
        uri <- parse[Token.Keyword.`uri`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        stringLiteral <- parse[Token.Type.StringLiteral]
      } yield Seq(uri, colon, stringLiteral),

      for {
        _ <- parse[Token.INDENT](2)
        method <- parse[Token.Keyword.`method`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        stringLiteral <- parse[Token.Type.StringLiteral]
      } yield Seq(method, colon, stringLiteral),

      for {
        _ <- parse[Token.INDENT](2)
        queryString <- parse[Token.Keyword.`queryString`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        dictionary <- parse[Token.Type.Dictionary]
      } yield Seq(queryString, colon, dictionary),

      for {
        _ <- parse[Token.INDENT](2)
        headers <- parse[Token.Keyword.`headers`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        dictionary <- parse[Token.Type.Dictionary]
      } yield Seq(headers, colon, dictionary),

      for {
        _ <- parse[Token.INDENT](2)
        body <- parse[Token.Keyword.`body`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        bodyType <- parseWebhookSettingBodyType
      } yield Seq(body, colon, bodyType)
    )
  }

  /*WebhookSettings ::= {WebhookSetting}*/
  private def parseWebhookSettings: Parsed[Option[ParseTree]] =  {
    repeat("WebhookSettings")(parseWebhookSetting)
  }

  /*Webhook ::= INDENT(1) '@webhook'
                          WebhookSettings*/
  private def parseWebhook: Parsed[ParseTree] = rule("Webhook") {
    for {
      _ <- parse[Token.INDENT](1)
      webhookKeyword <- parse[Token.Keyword.`@webhook`.type]
      webhookSettings <- parseWebhookSettings
    } yield Seq(webhookKeyword) |+| webhookSettings.toSeq
  }

  //Block ::= INDENT(2) '<scala>...<end>'
  private def parseBlock: Parsed[ParseTree] = rule("Block") {
    for {
      _ <- parse[Token.INDENT](2)
      scala <- parse[Token.Block.`<scala>`]
    } yield Seq(scala)
  }

  /*Process ::= INDENT(1) '@process'
                          Block*/
  private def parseProcess: Parsed[ParseTree] = rule("Process") {
    for {
      _ <- parse[Token.INDENT](1)
      process <- parse[Token.Keyword.`@process`.type]
      block <- parseBlock
    } yield Seq(process, block)
  }

  /*Rule ::= Description
             Constants
             Webhook
             Process*/
  private def parseRule: Parsed[ParseTree] = rule("Rule") {
    for {
      description <- parseDescription
      constants <- parseConstants
      webhook <- parseWebhook
      process <- parseProcess
    } yield Seq(description) |+| constants.toSeq |+| Seq(webhook) |+| Seq(process)
  }

  /*TopStat ::= Namespace
                Rule*/
  private def parseTopStat: Parsed[ParseTree] = rule("TopStat") {
    for {
      namespace <- parseNamespace
      rule <- parseRule
    } yield Seq(namespace, rule)
  }

  def parse(tokens: Seq[LexerToken]): SyntaxAnalysisFailed \/ ParseTree = {
    require(tokens.nonEmpty)

    parseTopStat.run(ParserStateInternal(tokens, 0, Map.empty))._2.leftMap(SyntaxAnalysisFailed(_))
  }
}