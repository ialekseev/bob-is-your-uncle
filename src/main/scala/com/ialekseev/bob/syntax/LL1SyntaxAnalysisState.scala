package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexicalAnalyzer._
import com.ialekseev.bob.syntax.SyntaxAnalyzer._
import scala.reflect.ClassTag
import scala.reflect._
import scalaz.Scalaz._
import scalaz._

private[syntax] trait LL1SyntaxAnalysisState {

  protected type ParserState[A] = State[ParserStateInternal, A]
  protected type Parsed[A] = ParserState[ParsingResult[A]]
  protected type ParsingResult[A] = \/[Seq[ParseError], A]

  protected type IndentLevel = Int
  protected type IndentLength = Int

  protected case class ParserStateInternal(val tokens: Seq[LexerToken], position: Int, indentMap: Map[IndentLevel, IndentLength])

  protected implicit def seqMonoid[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def zero: Seq[T] = Seq.empty[T]
    override def append(f1: Seq[T], f2: => Seq[T]): Seq[T] = f1 ++: f2
  }

  protected def current: ParserState[Option[LexerToken]] = get.map(s => {
    if (s.position < s.tokens.length) some(s.tokens(s.position))
    else none
  })

  protected def move: ParserState[Unit] = modify(s => s.copy(position = s.position + 1))
  protected def jump(to: Int): ParserState[Unit] = modify(s => s.copy(position = to))

  protected def rule(apply: => EitherT[ParserState, Seq[ParseError], Seq[ParseTree]]): Parsed[Seq[ParseTree]] = {
    get[ParserStateInternal] >>= (s => {
      val rollbackPosition = s.position
      val parsed: Parsed[Seq[ParseTree]] = apply.run
      parsed >>= {
        case \/-(nodes) => nodes.right[Seq[ParseError]].point[ParserState]
        case -\/(error) => jump(rollbackPosition) >> error.left[Seq[ParseTree]].point[ParserState]
      }
    })
  }

  protected def parseToken[T <: Token: ClassTag]: Parsed[LexerToken] = current >>= {
    case Some(t@LexerToken(_: T, _)) => move >| t.right
    case Some(LexerToken(token, offset)) => get[ParserStateInternal] >| Seq(ParseError(offset, s"Unexpected token: '$token' (expecting: '${classTag[T].runtimeClass.getSimpleName}')")).left
    case None => get[ParserStateInternal].map(s => Seq(ParseError(s.tokens.last.offset, "Unexpected end")).left)
  }

  protected def parseIndentToken(indentLevel: IndentLevel): Parsed[LexerToken] = {
    get[ParserStateInternal] >>= (state => {
      (for {
        indent <- EitherT.eitherT(parseToken[Token.INDENT])
        result: LexerToken <- EitherT.eitherT(
          if (state.indentMap.contains(indentLevel) && state.indentMap(indentLevel) === indent.token.length){
             indent.right[Seq[ParseError]].point[ParserState]
          } else if (!state.indentMap.contains(indentLevel) && (!state.indentMap.contains(indentLevel - 1) || (state.indentMap(indentLevel - 1) < indent.token.length))) {
              modify[ParserStateInternal](s => s.copy(indentMap = s.indentMap + (indentLevel -> indent.token.length))) >>
                indent.right[Seq[ParseError]].point[ParserState]
          } else Seq(ParseError(indent.offset, "Unexpected indent")).left[LexerToken].point[ParserState]
        )
      } yield result).run
    })
  }

  protected def repeat(ruleS: => Parsed[ParseTree], nonTerminalName: String): Parsed[Option[ParseTree]] = {
    def parse(nodes: Seq[ParseTree]): Parsed[Seq[ParseTree]] = {
      ruleS >>= {
        case \/-(node) => parse(nodes :+ node)
        case -\/(_) => nodes.right.point[ParserState]
      }
    }

    parse(Seq.empty[ParseTree]) >>= {
      case \/-(Seq()) => none.right.point[ParserState]
      case nodes@ \/-(_) => attachNodesToNonTerminal(nodes, nonTerminalName).map(some(_)).point[ParserState]
      case -\/(_) => sys.error("Not supposed to be here")
    }
  }

  protected def attachNodesToNonTerminal(nodesE: ParsingResult[Seq[ParseTree]], nonTerminalName: String): ParsingResult[ParseTree] = {
    nodesE.map(nodes => {
      Tree.Node(nonTerminal(nonTerminalName), nodes.toStream)
    })
  }

  protected def attachNodesToNonTerminal(nodesS: Parsed[Seq[ParseTree]], nonTerminalName: String): Parsed[ParseTree] = {
    nodesS.map(nodesE => attachNodesToNonTerminal(nodesE, nonTerminalName))
  }

  protected implicit class TokenSWrapper(tokenS: Parsed[LexerToken]) {
    def toTree: Parsed[ParseTree] = {
      tokenS.map(either => {
        either.map(token => terminal(token).leaf)
      })
    }
  }

  protected implicit class NodesSWrapper(nodesS: Parsed[Seq[ParseTree]]) {
    def attachToNonTerminal(nonTerminalName: String): Parsed[ParseTree] = {
      require(nonTerminalName.nonEmpty)

      attachNodesToNonTerminal(nodesS, nonTerminalName)
    }
  }
}
