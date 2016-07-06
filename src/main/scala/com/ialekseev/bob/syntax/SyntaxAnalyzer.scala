package com.ialekseev.bob.syntax

import com.ialekseev.bob.lexical.LexicalAnalyzer.LexerToken
import com.ialekseev.bob.syntax.SyntaxAnalyzer.{ParseError, ParseTree}
import scalaz._
import Scalaz._

/*[Example]
  namespace com.ialekseev.core#create
    description: "{description}"

    $var1: "{var1}"
    $var2: "{var2}"

    @webhook
      uri: "{binding}"
      method: "{method}"
      queryString: "{binding}"
*/

/*[Syntax in EBNF form]

    NamespacePathPart ::= '.' identifier
    NamespacePathParts ::= {NamespacePathPart}
    NamespacePath ::= identifier NamespacePathParts
    Namespace ::= INDENT(0) 'namespace' NamespacePath # identifier

    Description ::= INDENT(1) 'description' : stringLiteral

    Constant ::= INDENT(1) variable : stringLiteral
    Constants ::= {Constant}

    WebhookUriSetting ::= INDENT(2) 'uri' : stringLiteral

    WebhookSpecificSetting ::= INDENT(2) 'method' : stringLiteral |
                               INDENT(2) 'queryString' : dictionary

    WebhookSpecificSettings ::= {WebhookSpecificSetting}

    WebhookSettings ::= WebhookUriSetting
                        WebhookSpecificSettings

    Webhook ::= INDENT(1) '@webhook'
                          WebhookSettings

    Rule ::= Description
             Constants
             Webhook

    TopStat ::= Namespace
                Rule
*/


trait SyntaxAnalyzer {
  def parse(tokens: Seq[LexerToken]): \/[Seq[ParseError], ParseTree]
}

object SyntaxAnalyzer {
  type ParseTree = Tree[ParseTreeNode]

  case class ParseError(offset: Int, message: String)

  sealed trait ParseTreeNode
  case class Terminal(token: LexerToken) extends ParseTreeNode
  case class NonTerminal(name: String) extends ParseTreeNode

  def terminal(token: LexerToken): ParseTreeNode = Terminal(token)
  def nonTerminal(name: String): ParseTreeNode = NonTerminal(name)

  implicit val equalTerminal: Equal[Terminal] = Equal.equal(_ == _)
  implicit val equalNonTerminal: Equal[NonTerminal] = Equal.equal(_ == _)

  implicit val equalParseTreeNode: Equal[ParseTreeNode] = new Equal[ParseTreeNode] {
    def equal(a: ParseTreeNode, b: ParseTreeNode): Boolean = (a, b) match {
      case (aT: Terminal, bT: Terminal) => aT === bT
      case (anT: NonTerminal, bnT: NonTerminal) => anT === bnT
      case _ => false
    }
  }

  implicit val showParseTreeNode: Show[ParseTreeNode] = new Show[ParseTreeNode] {
    override def shows(node: ParseTreeNode): String = node match {
      case Terminal(token) => token.toString
      case NonTerminal(name) => "[" + name + "]"
    }
  }
}
