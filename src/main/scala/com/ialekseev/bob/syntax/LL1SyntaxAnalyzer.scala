package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexerToken
import scalaz._
import Scalaz._

//Top-Down Predictive Parsing for LL1 grammar (Recursive Descent technique)
class LL1SyntaxAnalyzer extends SyntaxAnalyzer {

  type ParserState[A] = State[ParserStateInternal, A]

  protected case class ParserStateInternal(val tokens: Seq[LexerToken], position: Int)
  protected case class ParserError(offset: Int, message: String)

  protected def lookAhead: ParserState[Option[LexerToken]] = get.map(s => {
    if (s.position < s.tokens.length - 1) Some(s.tokens(s.position + 1))
    else None
  })

  protected def current: ParserState[LexerToken] = get.map(s => s.tokens(s.position))

  def identifier(): ParserState[\/[ParserError, Token]] = current map {
    case LexerToken(t: Token.Identifier, _) => t.right
    case LexerToken(_, offset) => ParserError(offset, "Unexpected Identifier").left
  }

  //def namespacePath(): ParserState[RuleApplied] =
}
