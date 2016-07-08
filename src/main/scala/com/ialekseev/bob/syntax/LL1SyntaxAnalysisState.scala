package com.ialekseev.bob.syntax

import com.ialekseev.bob.{Token, TokenTag}
import com.ialekseev.bob.lexical.LexicalAnalyzer._
import com.ialekseev.bob.syntax.SyntaxAnalyzer._
import scala.collection.generic.SeqFactory
import scala.reflect.ClassTag
import scala.reflect._
import scalaz.Scalaz._
import scalaz._

private[syntax] trait LL1SyntaxAnalysisState {
  protected type Parsed[A] = EitherT[ParserState, Seq[ParseError], A]
  protected type ParserState[A] = State[ParserStateInternal, A]

  protected case class ParserStateInternal(val tokens: Seq[LexerToken], position: Int, indentMap: Map[IndentLevel, IndentLength])

  protected def rule(nonTerminalName: String)(apply: => Parsed[Seq[ParseTree]]): Parsed[ParseTree] = {
    require(nonTerminalName.nonEmpty)

    val parsed: Parsed[Seq[ParseTree]] = for {
      position <- EitherT.eitherT[ParserState, Seq[ParseError], Int](currentPosition.map(_.right[Seq[ParseError]]))
      applied <- (apply.swap >>= (error => EitherT.eitherT(jump(position) >| error.right[Seq[ParseTree]]))).swap
    } yield applied

    attachNodesToNonTerminal(parsed, nonTerminalName)
  }

  protected def parse[T <: Token: ClassTag](implicit tokenShow: Show[Token], tokenTag: TokenTag[T]): Parsed[ParseTree] = {
    parseToken[T].toTree
  }

  protected def parseIndent(indentLevel: IndentLevel): Parsed[ParseTree] = {
    parseIndentToken(indentLevel).toTree
  }

  //todo: trampoline
  protected def repeat(parsed: => Parsed[ParseTree], nonTerminalName: String): Parsed[Option[ParseTree]] = {
    def parse(nodes: Seq[ParseTree]): Parsed[Seq[ParseTree]] = {
      (parsed >>= (node => parse(nodes :+ node))) orElse nodes.asParsedRight
    }

    parse(Seq.empty[ParseTree]) >>= {
      case Seq() => none.asParsedRight
      case nodes@Seq(_, _*) => attachNodesToNonTerminal(nodes.asParsedRight, nonTerminalName).map(some(_))
    }
  }

  protected implicit def seqMonoid[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def zero: Seq[T] = Seq.empty[T]
    override def append(f1: Seq[T], f2: => Seq[T]): Seq[T] = f1 ++: f2
  }

  private type IndentLevel = Int
  private type IndentLength = Int

  private def current: ParserState[Option[LexerToken]] = get.map(s => {
    if (s.position < s.tokens.length) some(s.tokens(s.position))
    else none
  })
  private def currentPosition: ParserState[Int] = get.map(_.position)

  private def move: ParserState[Unit] = modify(s => s.copy(position = s.position + 1))
  private def jump(to: Int): ParserState[Unit] = modify(s => s.copy(position = to))

  private def parseToken[T <: Token : ClassTag](implicit tokenShow: Show[Token], tokenTag: TokenTag[T]): Parsed[LexerToken]= {
    EitherT.eitherT {
      current >>= {
        case Some(t@LexerToken(_: T, _)) => move >| t.right
        case Some(LexerToken(token, offset)) => get[ParserStateInternal] >| Seq(ParseError(offset, s"Unexpected: '${tokenShow.show(token)}' (expecting: '${tokenTag.asString}')")).left
        case None => get[ParserStateInternal].map(s => Seq(ParseError(s.tokens.last.offset, "Unexpected end")).left)
      }
    }
  }

  private def parseIndentToken(indentLevel: IndentLevel): Parsed[LexerToken] = {
    for {
      indent <- parseToken[Token.INDENT]
      result: LexerToken <- EitherT.eitherT[ParserState, Seq[ParseError], LexerToken] {
        get[ParserStateInternal] >>= (state => {
          if (state.indentMap.contains(indentLevel) && state.indentMap(indentLevel) === indent.token.length){
            get[ParserStateInternal] >| indent.right
            indent.asParserStateRight
          } else if (!state.indentMap.contains(indentLevel) && (!state.indentMap.contains(indentLevel - 1) || (state.indentMap(indentLevel - 1) < indent.token.length))) {
            modify[ParserStateInternal](s => s.copy(indentMap = s.indentMap + (indentLevel -> indent.token.length))) >>
              indent.asParserStateRight
          } else Seq(ParseError(indent.offset, s"Unexpected indent width: ${indent.token.length}")).asParserStateLeft[LexerToken]
        })
      }
    } yield result
  }

  private def attachNodesToNonTerminal(parsed: Parsed[Seq[ParseTree]], nonTerminalName: String): Parsed[ParseTree] = {
    for {
      nodes <- parsed
      nonTerminal <- Tree.Node(nonTerminal(nonTerminalName), nodes.toStream).asParsedRight
    } yield nonTerminal
  }

  private implicit class ParsedTokenWrapper(parsed: Parsed[LexerToken]) {
    def toTree: Parsed[ParseTree] = {
      for {
        token <- parsed
        terminal <- terminal(token).leaf.asParsedRight
      } yield terminal
    }
  }

  private implicit class EverythingWrapper[T](obj: T) {
    def asParsedRight: Parsed[T] = EitherT.eitherT(obj.asParserStateRight)
    def asParserStateRight: ParserState[\/[Seq[ParseError], T]] = obj.right[Seq[ParseError]].asParserState
    def asParserStateLeft[R]: ParserState[\/[T, R]] = obj.left[R].asParserState
    def asParserState: ParserState[T] = obj.point[ParserState]
  }
}
