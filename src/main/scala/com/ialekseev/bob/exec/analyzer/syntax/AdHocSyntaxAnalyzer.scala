package com.ialekseev.bob.exec.analyzer.syntax

import com.ialekseev.bob.SyntaxAnalysisFailed
import com.ialekseev.bob.exec.analyzer.syntax.SyntaxAnalyzer._
import com.ialekseev.bob.exec.analyzer.{LexerToken, Token}
import scalaz.Scalaz._
import scalaz._

//Top-Down Predictive Parsing for LL grammar (Recursive Descent technique)
class AdHocSyntaxAnalyzer extends SyntaxAnalyzer with SyntaxAnalysisState {

  //NamespacePathPart ::= '.' identifier
  private def parseNamespacePathPart: Parsed[ParseTree] = rule("NamespacePathPart") {
    for {
      dot <- parse[Token.Delimiter.`.`.type]
      identifier <- parse[Token.Identifier]
    } yield List(dot, identifier)
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
    } yield identifier +: dotPath.toList
  }

  //Namespace ::= INDENT(0) 'namespace' NamespacePath # identifier
  private def parseNamespace: Parsed[ParseTree] = rule("Namespace") {
    for {
      _ <- parse[Token.INDENT](0)
      namespaceKeyword <- parse[Token.Keyword.`namespace`.type]
      namespacePath <- parseNamespacePath
      pound <- parse[Token.Delimiter.`#`.type]
      identifier <- parse[Token.Identifier]
    } yield List(namespaceKeyword, namespacePath, pound, identifier)
  }

  //Description ::= INDENT(1) 'description' : stringLiteral
  private def parseDescription: Parsed[ParseTree] = rule("Description") {
    for {
      _ <- parse[Token.INDENT](1)
      descriptionKeyword <- parse[Token.Keyword.`description`.type]
      colon <- parse[Token.Delimiter.`:`.type]
      stringLiteral <- parse[Token.Type.StringLiteral]
    } yield List(descriptionKeyword, colon, stringLiteral)
  }

  //Constant ::= INDENT(1) variable : stringLiteral
  private def parseConstant: Parsed[ParseTree] = rule("Constant") {
    for {
      _ <- parse[Token.INDENT](1)
      variable <- parse[Token.Variable]
      colon <- parse[Token.Delimiter.`:`.type]
      stringLiteral <- parse[Token.Type.StringLiteral]
    } yield List(variable, colon, stringLiteral)
  }

  //Constants ::= {Constant}
  private def parseConstants: Parsed[Option[ParseTree]] = {
    repeat("Constants")(parseConstant)
  }

  //WebhookSettingBodyType ::= 'stringLiteral' | 'dictionary' | 'json'
  private def parseWebhookSettingBodyType: Parsed[ParseTree] = {
    or("WebhookSettingBodyType")("Expecting some valid Body type here")(
      for(stringLiteral <- parse[Token.Type.StringLiteral]) yield List(stringLiteral),
      for(dictionary <- parse[Token.Type.Dictionary]) yield List(dictionary),
      for(json <- parse[Token.Type.Json]) yield List(json)
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
      } yield List(uri, colon, stringLiteral),

      for {
        _ <- parse[Token.INDENT](2)
        method <- parse[Token.Keyword.`method`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        stringLiteral <- parse[Token.Type.StringLiteral]
      } yield List(method, colon, stringLiteral),

      for {
        _ <- parse[Token.INDENT](2)
        queryString <- parse[Token.Keyword.`queryString`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        dictionary <- parse[Token.Type.Dictionary]
      } yield List(queryString, colon, dictionary),

      for {
        _ <- parse[Token.INDENT](2)
        headers <- parse[Token.Keyword.`headers`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        dictionary <- parse[Token.Type.Dictionary]
      } yield List(headers, colon, dictionary),

      for {
        _ <- parse[Token.INDENT](2)
        body <- parse[Token.Keyword.`body`.type]
        colon <- parse[Token.Delimiter.`:`.type]
        bodyType <- parseWebhookSettingBodyType
      } yield List(body, colon, bodyType)
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
    } yield List(webhookKeyword) |+| webhookSettings.toList
  }

  //Block ::= INDENT(2) '<scala>...<end>'
  private def parseBlock: Parsed[ParseTree] = rule("Block") {
    for {
      _ <- parse[Token.INDENT](2)
      scala <- parse[Token.Block.`<scala>`]
    } yield List(scala)
  }

  /*Process ::= INDENT(1) '@process'
                          Block*/
  private def parseProcess: Parsed[ParseTree] = rule("Process") {
    for {
      _ <- parse[Token.INDENT](1)
      process <- parse[Token.Keyword.`@process`.type]
      block <- parseBlock
    } yield List(process, block)
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
    } yield List(description) |+| constants.toList |+| List(webhook) |+| List(process)
  }

  /*TopStat ::= Namespace
                Rule*/
  private def parseTopStat: Parsed[ParseTree] = rule("TopStat") {
    for {
      namespace <- parseNamespace
      rule <- parseRule
    } yield List(namespace, rule)
  }

  def parse(tokens: List[LexerToken]): SyntaxAnalysisFailed \/ ParseTree = {
    require(tokens.nonEmpty)

    parseTopStat.run(ParserStateInternal(tokens, 0, Map.empty))._2.leftMap(SyntaxAnalysisFailed(_))
  }
}