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
      dot <- EitherT.eitherT(parse[Token.Delimiter.`.`.type])
      identifier <- EitherT.eitherT(parse[Token.Identifier])
    } yield Seq(dot, identifier)).run

    attachToNonTerminal(parsed, "NamespacePathPart")
  }

  //NamespacePathParts ::= {NamespacePathPart}
  private def parseNamespacePathParts: Parsed[Option[ParseTree]] = {
    parseRepeatable(parseNamespacePathPart, "NamespacePathParts")
  }

  //NamespacePath ::= identifier NamespacePathParts
  private def parseNamespacePath: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      identifier <- EitherT.eitherT(parse[Token.Identifier])
      dotPath <- EitherT.eitherT(parseNamespacePathParts)
    } yield identifier +: dotPath.toSeq).run

    attachToNonTerminal(parsed, "NamespacePath")
  }

  //Namespace ::= 'namespace' NamespacePath # identifier
  private def parseNamespace: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      namespaceKeyword <- EitherT.eitherT(parse[Token.Keyword.`namespace`.type])
      namespacePath <- EitherT.eitherT(parseNamespacePath)
      pound <- EitherT.eitherT(parse[Token.Delimiter.`#`.type])
      identifier <- EitherT.eitherT(parse[Token.Identifier])
    } yield Seq(namespaceKeyword, namespacePath, pound, identifier)).run

    attachToNonTerminal(parsed, "Namespace")
  }

  //Description ::= 'description' : stringLiteral
  private def parseDescription: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      descriptionKeyword <- EitherT.eitherT(parse[Token.Keyword.`description`.type])
      colon <- EitherT.eitherT(parse[Token.Delimiter.`:`.type])
      stringLiteral <- EitherT.eitherT(parse[Token.StringLiteral])
    } yield Seq(descriptionKeyword, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "Description")
  }

  //Constant ::= variable : stringLiteral
  private def parseConstant: Parsed[ParseTree] = {

    var parsed: Parsed[Seq[ParseTree]] = (for {
      variable <- EitherT.eitherT(parse[Token.Variable])
      colon <- EitherT.eitherT(parse[Token.Delimiter.`:`.type])
      stringLiteral <- EitherT.eitherT(parse[Token.StringLiteral])
    } yield Seq(variable, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "Constant")
  }

  //Constants ::= {Constant}
  private def parseConstants: Parsed[Option[ParseTree]] = {
    parseRepeatable(parseConstant, "Constants")
  }

  //WebhookUriSetting ::= 'uri' : stringLiteral
  private def parseWebhookUriSetting: Parsed[ParseTree] = {
    val parsed: Parsed[Seq[ParseTree]] = (for {
      variable <- EitherT.eitherT(parse[Token.Variable])
      colon <- EitherT.eitherT(parse[Token.Delimiter.`:`.type])
      stringLiteral <- EitherT.eitherT(parse[Token.StringLiteral])
    } yield Seq(variable, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "WebhookUriSetting")
  }

  /*WebhookSpecificSetting ::= 'method' : stringLiteral |
                               'queryString' : stringLiteral*/
  private def parseWebhookSpecificSetting: Parsed[ParseTree] = {

    val keywordT = EitherT.eitherT(parse[Token.Keyword.`method`.type]) orElse
                 EitherT.eitherT(parse[Token.Keyword.`queryString`.type])

    val parsed: Parsed[Seq[ParseTree]] = (for {
      keyword <- keywordT
      colon <- EitherT.eitherT(parse[Token.Delimiter.`:`.type])
      stringLiteral <- EitherT.eitherT(parse[Token.StringLiteral])
    } yield Seq(keyword, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "WebhookSpecificSetting")
  }

  //WebhookSpecificSettings ::= {WebhookSpecificSetting}
  private def parseWebhookSpecificSettings: Parsed[Option[ParseTree]] = {
    parseRepeatable(parseWebhookSpecificSetting, "WebhookSpecificSettings")
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

  /*Webhook ::= '@webhook'
                  WebhookSettings*/
  private def parseWebhook: Parsed[ParseTree] = {

    val parsed: Parsed[Seq[ParseTree]] = (for {
      webhookKeyword <- EitherT.eitherT(parse[Token.Keyword.`@webhook`.type])
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

    parseTopStat.run(ParserStateInternal(tokens, 0, 0))._2
  }
}