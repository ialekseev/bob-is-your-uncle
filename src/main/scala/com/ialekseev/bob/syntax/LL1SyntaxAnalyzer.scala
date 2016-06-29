package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexerToken
import scalaz._
import Scalaz._
import scala.reflect.ClassTag

//Top-Down Predictive Parsing for LL1 grammar (Recursive Descent technique)
class LL1SyntaxAnalyzer extends SyntaxAnalyzer with LL1SyntaxAnalysisState {

  //NamespacePathPart ::= '.' identifier
  private def parseNamespacePathPart: Parsed[PTree] = {

    val parsed: Parsed[Seq[PTree]] = (for {
      dot <- EitherT.eitherT(parse[Token.Delimiter.`.`.type])
      identifier <- EitherT.eitherT(parse[Token.Identifier])
    } yield Seq(dot, identifier)).run

    attachToNonTerminal(parsed, "NamespacePathPart")
  }

  //NamespacePathParts ::= {NamespacePathPart}
  private def parseNamespacePathParts: Parsed[Option[PTree]] = parseRepeatable(parseNamespacePathPart, "NamespacePathParts")

  //NamespacePath ::= identifier NamespacePathPart
  private def parseNamespacePath: Parsed[PTree] = {

    def parseDotPath(nodes: Seq[PTree]): Parsed[Seq[PTree]] = {
      parse[Token.Delimiter.`.`.type] >>= {
        case \/-(dot) => parse[Token.Identifier] >>= {
          case \/-(identifier) => parseDotPath(nodes :+ dot :+ identifier)
          case -\/(err) => err.left.point[ParserState]
        }
        case -\/(_) => nodes.right.point[ParserState]
      }
    }

    val parsed: Parsed[Seq[PTree]] = (for {
      identifier: Seq[PTree] <- EitherT.eitherT(liftToSeq(parse[Token.Identifier]))
      dotPath: Seq[PTree] <- EitherT.eitherT(parseDotPath(Seq.empty[PTree]))
    } yield identifier |+| dotPath).run

    attachToNonTerminal(parsed, "NamespacePath")
  }

  //Namespace ::= 'namespace' NamespacePath # identifier
  private def parseNamespace: Parsed[PTree] = {

    val parsed: Parsed[Seq[PTree]] = (for {
      namespaceKeyword <- EitherT.eitherT(parse[Token.Keyword.`namespace`.type])
      namespacePath <- EitherT.eitherT(parseNamespacePath)
      pound <- EitherT.eitherT(parse[Token.Delimiter.`#`.type])
      identifier <- EitherT.eitherT(parse[Token.Identifier])
    } yield Seq(namespaceKeyword, namespacePath, pound, identifier)).run

    attachToNonTerminal(parsed, "Namespace")
  }

  //Description ::= 'description' : stringLiteral
  private def parseDescription: Parsed[PTree] = {

    val parsed: Parsed[Seq[PTree]] = (for {
      descriptionKeyword <- EitherT.eitherT(parse[Token.Keyword.`description`.type])
      colon <- EitherT.eitherT(parse[Token.Delimiter.`:`.type])
      stringLiteral <- EitherT.eitherT(parse[Token.StringLiteral])
    } yield Seq(descriptionKeyword, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "Description")
  }

  //Constant ::= variable : stringLiteral
  private def parseConstant: Parsed[PTree] = {

    var parsed: Parsed[Seq[PTree]] = (for {
      variable <- EitherT.eitherT(parse[Token.Variable])
      colon <- EitherT.eitherT(parse[Token.Delimiter.`:`.type])
      stringLiteral <- EitherT.eitherT(parse[Token.StringLiteral])
    } yield Seq(variable, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "Constant")
  }

  //Constants ::= {Constant}
  private def parseConstants: Parsed[Option[PTree]] = parseRepeatable(parseConstant, "Constants")

  //WebhookUriSetting ::= 'uri' : stringLiteral
  private def parseWebhookUriSetting: Parsed[PTree] = {
    val parsed: Parsed[Seq[PTree]] = (for {
      variable <- EitherT.eitherT(parse[Token.Variable])
      colon <- EitherT.eitherT(parse[Token.Delimiter.`:`.type])
      stringLiteral <- EitherT.eitherT(parse[Token.StringLiteral])
    } yield Seq(variable, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "WebhookUriSetting")
  }

  /*WebhookSpecificSetting ::= 'method' : stringLiteral |
                               'queryString' : stringLiteral*/
  private def parseWebhookSpecificSetting: Parsed[PTree] = {

    val keywordT = EitherT.eitherT(parse[Token.Keyword.`method`.type]) orElse
                 EitherT.eitherT(parse[Token.Keyword.`queryString`.type])

    val parsed: Parsed[Seq[PTree]] = (for {
      keyword <- keywordT
      colon <- EitherT.eitherT(parse[Token.Delimiter.`:`.type])
      stringLiteral <- EitherT.eitherT(parse[Token.StringLiteral])
    } yield Seq(keyword, colon, stringLiteral)).run

    attachToNonTerminal(parsed, "WebhookSpecificSetting")
  }

  //WebhookSpecificSettings ::= {WebhookSpecificSetting}
  private def parseWebhookSpecificSettings: Parsed[Option[PTree]] = parseRepeatable(parseWebhookSpecificSetting, "WebhookSpecificSettings")

  /*WebhookSettings ::= WebhookUriSetting
                        {WebhookSpecificSetting}*/
  private def parseWebhookSettings: Parsed[PTree] = {

    val parsed: Parsed[Seq[PTree]] = (for {
      uriSetting <- EitherT.eitherT(parseWebhookUriSetting)
      specificSettings <- EitherT.eitherT(parseWebhookSpecificSettings)
    } yield uriSetting +: specificSettings.toSeq).run

    attachToNonTerminal(parsed, "WebhookSettings")
  }
}
