package com.ialekseev.bob.syntax

import com.ialekseev.bob.{Token, TokenTag}
import com.ialekseev.bob.lexical.LexicalAnalyzer._
import com.ialekseev.bob.syntax.LLSyntaxAnalyzer._
import scala.collection.generic.SeqFactory
import scala.reflect.ClassTag
import scala.reflect._
import scalaz.Scalaz._
import scalaz._

private[syntax] trait LLSyntaxAnalysisState {
  protected type Parsed[R] = EitherT[ParserState, Seq[ParseError], R]
  protected type ParserState[S] = State[ParserStateInternal, S]

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

  protected def parse[T <: Token.INDENT](indentLevel: IndentLevel): Parsed[ParseTree] = {
    (for {
      indent <- parseToken[Token.INDENT]
      result: LexerToken <- EitherT.eitherT[ParserState, Seq[ParseError], LexerToken] {
        get[ParserStateInternal] >>= (state => {
          if (state.indentMap.contains(indentLevel) && state.indentMap(indentLevel) === indent.token.length){
            get[ParserStateInternal] >| indent.right
            indent.right.point[ParserState]
          } else if (!state.indentMap.contains(indentLevel) && (!state.indentMap.contains(indentLevel - 1) || (state.indentMap(indentLevel - 1) < indent.token.length))) {
            modify[ParserStateInternal](s => s.copy(indentMap = s.indentMap + (indentLevel -> indent.token.length))) >>
              indent.right.point[ParserState]
          } else Seq(ParseError(indent.offset, state.position, s"Unexpected indent width: ${indent.token.length}")).left.point[ParserState]
        })
      }
    } yield result).toTree
  }

  //todo: trampoline
  protected def repeat(parsed: => Parsed[ParseTree], nonTerminalName: String): Parsed[Option[ParseTree]] = {
    def go(nodes: Seq[ParseTree]): EitherT[ParserState, (ParseError, Seq[ParseTree]), Seq[ParseTree]] = {
      for {
        node <- parsed.leftMap(errors => (errors.head, nodes))
        result <- go(nodes :+ node)
      } yield result
    }

    for {
      gone: (ParseError, Seq[ParseTree]) <- go(Seq.empty[ParseTree]).swap.leftMap(_ => Seq.empty[ParseError])
      currentPosition: Int <- EitherT.eitherT[ParserState, Seq[ParseError], Int](currentPosition.map(_.right[Seq[ParseError]]))
      result: Option[ParseTree] <- {
        val errorTokenPosition = gone._1.tokenIndex
        if (errorTokenPosition <= currentPosition + 1) {
          gone._2 match {
            case Seq() => EitherT.eitherT(none.right[Seq[ParseError]].point[ParserState])
            case nodes@Seq(_, _*) => attachNodesToNonTerminal(EitherT.eitherT(gone._2.right.point[ParserState]), nonTerminalName).map(some(_))
          }
        }
        else EitherT.eitherT(Seq(gone._1).left[Option[ParseTree]].point[ParserState])
      }
    } yield result
  }

  protected def optional(parsed: => Parsed[Seq[ParseTree]]): Parsed[Option[Seq[ParseTree]]] = {
    //todo: implement by analogy with 'repeat'
    ???
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
        case Some(LexerToken(token, offset)) => currentPosition.map(position => Seq(ParseError(offset, position, s"Unexpected: '${tokenShow.show(token)}' (expecting: '${tokenTag.asString}')")).left)
        case None => get[ParserStateInternal].map(s => Seq(ParseError(s.tokens.last.offset, s.position, "Unexpected end")).left)
      }
    }
  }

  private def attachNodesToNonTerminal(parsed: Parsed[Seq[ParseTree]], nonTerminalName: String): Parsed[ParseTree] = {
    for {
      nodes <- parsed
      nonTerminal <- EitherT.eitherT(Tree.Node(nonTerminal(nonTerminalName), nodes.toStream).right[Seq[ParseError]].point[ParserState])
    } yield nonTerminal
  }

  private implicit class ParsedTokenWrapper(parsed: Parsed[LexerToken]) {
    def toTree: Parsed[ParseTree] = {
      for {
        token <- parsed
        terminal <- EitherT.eitherT(terminal(token).leaf.right[Seq[ParseError]].point[ParserState])
      } yield terminal
    }
  }
}
