package com.ialekseev.bob.syntax

import com.ialekseev.bob.lexical.LexerToken
import scalaz.Scalaz._
import scalaz._

private[syntax] trait LL1SyntaxAnalysisState {

  protected type ParserState[A] = State[ParserStateInternal, A]
  protected type Parsed[A] = \/[Seq[ParserError], A]

  protected case class ParserStateInternal(val tokens: Seq[LexerToken], position: Int)
  protected case class ParserError(offset: Int, message: String)

  protected sealed trait Node
  protected case class Terminal(token: LexerToken) extends Node
  protected case class NonTerminal(name: String) extends Node

  implicit def seqMonoid[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
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

  protected def liftToSeq(state: ParserState[Parsed[Tree[Node]]]): ParserState[Parsed[Seq[Tree[Node]]]] = {
    state.map {
      case \/-(node) => Seq(node).right
      case  e@ -\/(_) => e
    }
  }
}
