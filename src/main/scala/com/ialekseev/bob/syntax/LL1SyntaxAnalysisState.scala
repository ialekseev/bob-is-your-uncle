package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexerToken
import scala.reflect.ClassTag
import scalaz.Scalaz._
import scalaz._

private[syntax] trait LL1SyntaxAnalysisState {

  protected type ParserState[A] = State[ParserStateInternal, A]
  protected type Parsed[A] = ParserState[ParsingResult[A]]
  protected type ParsingResult[A] = \/[Seq[ParserError], A]
  protected type PTree = Tree[Node]

  protected case class ParserStateInternal(val tokens: Seq[LexerToken], position: Int)
  protected case class ParserError(offset: Int, message: String)

  protected sealed trait Node
  protected case class Terminal(token: LexerToken) extends Node
  protected case class NonTerminal(name: String) extends Node

  protected implicit def seqMonoid[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def zero: Seq[T] = Seq.empty[T]
    override def append(f1: Seq[T], f2: => Seq[T]): Seq[T] = f1 ++: f2
  }

  protected def lookAhead: ParserState[Option[LexerToken]] = get.map(s => {
    if (s.position < s.tokens.length - 1) some(s.tokens(s.position + 1))
    else none
  })

  protected def current: ParserState[Option[LexerToken]] = get.map(s => {
    if (s.position < s.tokens.length) some(s.tokens(s.position))
    else none
  })

  //private def current: ParserState[LexerToken] = get.map(s => s.tokens(s.position))
  //private def hasCurrent: ParserState[Boolean] = get.map(s => s.position < s.tokens.length)

  protected def move: ParserState[Unit] = modify(s => s.copy(position = s.position + 1))

  protected def parse[T <: Token: ClassTag]: Parsed[PTree] = current >>= {
    case Some(t@LexerToken(_: T, _)) => move >| (Terminal(t): Node).leaf.right
    case Some(LexerToken(_, offset)) => get[ParserStateInternal] >| Seq(ParserError(offset, "Unexpected word")).left
    case None => get[ParserStateInternal].map(s => Seq(ParserError(s.tokens.last.offset, "Unexpected end")).left)
  }

  protected def parseRepeatable(rule: Parsed[PTree], nonTerminalName: String): Parsed[Option[PTree]] = {
    def parse(nodes: Seq[PTree]): Parsed[Seq[PTree]] = {
      rule >>= {
        case \/-(node) => parse(nodes :+ node)
        case -\/(_) => nodes.right.point[ParserState]
      }
    }

    parse(Seq.empty[PTree]) >>= {
      case \/-(Seq()) => none.right.point[ParserState]
      case nodes@ \/-(_) => attachToNonTerminal(nodes, nonTerminalName).map(some(_)).point[ParserState]
      case -\/(_) => sys.error("Not supposed to be here")
    }
  }

  protected def liftToSeq(nodeS: Parsed[PTree]): Parsed[Seq[PTree]] = {
    nodeS.map {
      case \/-(node) => Seq(node).right
      case  e@ -\/(_) => e
    }
  }

  protected def attachToNonTerminal(nodesE: ParsingResult[Seq[PTree]], nonTerminalName: String): ParsingResult[PTree] = {
    nodesE.map(nodes => {
      Tree.Node(NonTerminal(nonTerminalName), nodes.toStream)
    })
  }

  protected def attachToNonTerminal(nodesS: Parsed[Seq[PTree]], nonTerminalName: String): Parsed[PTree] = {
    nodesS.map(nodesE => attachToNonTerminal(nodesE, nonTerminalName))
  }
}
