package com.ialekseev.bob.analyzer.syntax

import com.ialekseev.bob.Models.SyntaxAnalysisFailed
import com.ialekseev.bob.analyzer.LexerToken
import com.ialekseev.bob.analyzer.syntax.SyntaxAnalyzer.ParseTree

import scalaz.Scalaz._
import scalaz._

/*[Example]
  namespace com.ialekseev.core#create
    description: "{description}"

    $var1: "..."
    $var2: "..."

    @webhook
      uri: "..."
      method: "..."
      headers: "..."
      queryString: "..."
      body: "..."

    @process
      <scala>
        ...
      <end>
*/

/*[Syntax in EBNF form (LL Grammar)]

    NamespacePathPart ::= '.' identifier
    NamespacePathParts ::= {NamespacePathPart}
    NamespacePath ::= identifier NamespacePathParts
    Namespace ::= INDENT(0) 'namespace' NamespacePath # identifier

    Description ::= INDENT(1) 'description' : stringLiteral

    Constant ::= INDENT(1) variable : stringLiteral
    Constants ::= {Constant}

    WebhookSettingBodyType ::= 'stringLiteral' | 'dictionary' | 'json'

    WebhookSetting ::= INDENT(2) 'uri' : stringLiteral |
                       INDENT(2) 'method' : stringLiteral |
                       INDENT(2) 'headers' : dictionary |
                       INDENT(2) 'queryString' : dictionary |
                       INDENT(2) 'body': WebhookSettingBodyType

    WebhookSettings ::= {WebhookSetting}

    Webhook ::= INDENT(1) '@webhook'
                          WebhookSettings

    Block ::= INDENT(2) '<scala>...<end>'

    Process ::= INDENT(1) '@process'
                          Block

    Rule ::= Description
             Constants
             Webhook
             Process

    TopStat ::= Namespace
                Rule
*/


trait SyntaxAnalyzer {
  def parse(tokens: Seq[LexerToken]): SyntaxAnalysisFailed \/ ParseTree
}

object SyntaxAnalyzer {
  type ParseTree = Tree[ParseTreeNode]

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
