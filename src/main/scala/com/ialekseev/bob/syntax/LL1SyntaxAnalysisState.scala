package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexicalAnalyzer._
import com.ialekseev.bob.syntax.SyntaxAnalyzer._
import scala.reflect.ClassTag
import scalaz.Scalaz._
import scalaz._

private[syntax] trait LL1SyntaxAnalysisState {

  protected type ParserState[A] = State[ParserStateInternal, A]
  protected type Parsed[A] = ParserState[ParsingResult[A]]
  protected type ParsingResult[A] = \/[Seq[ParseError], A]

  protected case class ParserStateInternal(val tokens: Seq[LexerToken], position: Int, indent: Int)

  protected implicit def seqMonoid[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def zero: Seq[T] = Seq.empty[T]
    override def append(f1: Seq[T], f2: => Seq[T]): Seq[T] = f1 ++: f2
  }

  protected def current: ParserState[Option[LexerToken]] = get.map(s => {
    if (s.position < s.tokens.length) some(s.tokens(s.position))
    else none
  })

  protected def move: ParserState[Unit] = modify(s => s.copy(position = s.position + 1))
  protected def setIndent(i: Int): ParserState[Unit] = modify(s => s.copy(indent = i))

  protected def parseToken[T <: Token: ClassTag]: Parsed[LexerToken] = current >>= {
    case Some(t@LexerToken(_: T, _)) => move >| t.right
    case Some(LexerToken(_, offset)) => get[ParserStateInternal] >| Seq(ParseError(offset, "Unexpected word")).left
    case None => get[ParserStateInternal].map(s => Seq(ParseError(s.tokens.last.offset, "Unexpected end")).left)
  }

  protected def parse[T <: Token: ClassTag]: Parsed[ParseTree] = mapTokenToParseTree(parseToken[T])

  protected def parseBlockIndent: Parsed[ParseTree] = {
   val parsedToken: Parsed[LexerToken] = (for {
      indent <- EitherT.eitherT(parseToken[Token.INDENT])
      _ <- EitherT.eitherT(setIndent(indent.token.length) >| indent.right[Seq[ParseError]])
    } yield indent).run

    mapTokenToParseTree(parsedToken)
  }

  protected def parseRepeatable(ruleS: => Parsed[ParseTree], nonTerminalName: String): Parsed[Option[ParseTree]] = {
    def parse(nodes: Seq[ParseTree]): Parsed[Seq[ParseTree]] = {
      ruleS >>= {
        case \/-(node) => parse(nodes :+ node)
        case -\/(_) => nodes.right.point[ParserState]
      }
    }

    parse(Seq.empty[ParseTree]) >>= {
      case \/-(Seq()) => none.right.point[ParserState]
      case nodes@ \/-(_) => attachToNonTerminal(nodes, nonTerminalName).map(some(_)).point[ParserState]
      case -\/(_) => sys.error("Not supposed to be here")
    }
  }

  protected def attachToNonTerminal(nodesE: ParsingResult[Seq[ParseTree]], nonTerminalName: String): ParsingResult[ParseTree] = {
    nodesE.map(nodes => {
      Tree.Node(NonTerminal(nonTerminalName), nodes.toStream)
    })
  }

  protected def attachToNonTerminal(nodesS: Parsed[Seq[ParseTree]], nonTerminalName: String): Parsed[ParseTree] = {
    nodesS.map(nodesE => attachToNonTerminal(nodesE, nonTerminalName))
  }

  private def mapTokenToParseTree(tokenS: Parsed[LexerToken]): Parsed[ParseTree] = {
    tokenS.map(either => {
      either.map(token => (Terminal(token): ParseTreeNode).leaf)
    })
  }
}
