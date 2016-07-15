package com.ialekseev.bob.syntax

import com.ialekseev.bob.{Token, TokenTag, LexerToken, LexerError}
import com.ialekseev.bob.syntax.LLSyntaxAnalyzer._
import scala.collection.generic.SeqFactory
import scala.reflect.ClassTag
import scala.reflect._
import scalaz.Scalaz._
import scalaz._

private[syntax] trait LLSyntaxAnalysisState {
  type Parsed[R] = EitherT[ParserState, Seq[ParseError], R]
  type ParserState[S] = State[ParserStateInternal, S]
  type IndentLevel = Int
  type IndentLength = Int

  case class ParserStateInternal(val tokens: Seq[LexerToken], position: Int, indentMap: Map[IndentLevel, IndentLength])

  def current: ParserState[(Int, Option[LexerToken])] = get.map(s => {
    if (s.position < s.tokens.length) (s.position, some(s.tokens(s.position)))
    else (s.position, none)
  })
  def currentT: Parsed[(Int, Option[LexerToken])] = EitherT.eitherT[ParserState, Seq[ParseError], (Int, Option[LexerToken])](current.map(_.right[Seq[ParseError]]))

  def previous: ParserState[(Int, LexerToken)] = get.map(s => (s.position - 1, s.tokens(s.position - 1)))
  def previousT: Parsed[(Int, LexerToken)] = EitherT.eitherT[ParserState, Seq[ParseError], (Int, LexerToken)](previous.map(_.right[Seq[ParseError]]))

  def move: ParserState[Unit] = modify(s => s.copy(position = s.position + 1))
  def jump(to: Int): ParserState[Unit] = modify(s => s.copy(position = to))

  def parseToken[T <: Token : ClassTag](implicit tokenShow: Show[Token], tokenTag: TokenTag[T]): Parsed[LexerToken]= {
    EitherT.eitherT {
      current >>= {
        case (_, Some(t@LexerToken(_: T, _))) => move >| t.right
        case (position, Some(LexerToken(token, offset))) => Seq(ParseError(offset, position, s"Unexpected: '${tokenShow.show(token)}' (expecting: '${tokenTag.asString}')")).left.point[ParserState]
        case (_, None) => get[ParserStateInternal].map(s => Seq(ParseError(s.tokens.last.offset, s.position, "Unexpected end")).left)
      }
    }
  }

  def parse[T <: Token: ClassTag](implicit tokenShow: Show[Token], tokenTag: TokenTag[T]): Parsed[ParseTree] = parseToken[T].toTree
  def parse[T <: Token.INDENT](indentLevel: IndentLevel): Parsed[ParseTree] = {
    require(indentLevel >= 0)

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
          } else Seq(ParseError(indent.offset, state.position - 1, s"Unexpected indent width: ${indent.token.length}")).left.point[ParserState]
        })
      }
    } yield result).toTree
  }

  def applyOrRollback(apply: Parsed[Seq[ParseTree]]): Parsed[Seq[ParseTree]]  = {
    for {
      (position, _) <- currentT
      applied <- (apply.swap >>= (error => EitherT.eitherT(jump(position) >| error.right[Seq[ParseTree]]))).swap
    } yield applied
  }

  def rule(nonTerminalName: String)(apply:  Parsed[Seq[ParseTree]]): Parsed[ParseTree] = {
    require(nonTerminalName.nonEmpty)

    attachNodesToNonTerminal(applyOrRollback(apply), nonTerminalName)
  }

  //todo: trampoline?
  def repeat(nonTerminalName: String)(parsed: => Parsed[ParseTree]): Parsed[Option[ParseTree]] = {
    require(nonTerminalName.nonEmpty)

    def go(nodes: Seq[ParseTree]): EitherT[ParserState, (ParseError, Seq[ParseTree]), Seq[ParseTree]] = {
      for {
        node <- parsed.leftMap(errors => (errors.head, nodes))
        result <- go(nodes :+ node)
      } yield result
    }

    for {
      done: (ParseError, Seq[ParseTree]) <- go(Seq.empty[ParseTree]).swap.leftMap(_ => Seq.empty[ParseError])
      (currentPosition, _) <- currentT
      result: Option[ParseTree] <- {
        if (isErrorJustMeansWrongRuleApplication(currentPosition, done._1.tokenIndex)) {
          done._2 match {
            case Seq() => EitherT.eitherT(none.right[Seq[ParseError]].point[ParserState])
            case nodes@Seq(_, _*) => attachNodesToNonTerminal(EitherT.eitherT(done._2.right.point[ParserState]), nonTerminalName).map(some(_))
          }
        }
        else EitherT.eitherT(Seq(done._1).left[Option[ParseTree]].point[ParserState])
      }
    } yield result
  }

  def or(nonTerminalName: String)(absenceMessage: String)(one: Parsed[Seq[ParseTree]], others: Parsed[Seq[ParseTree]]*): Parsed[ParseTree] = {
    require(nonTerminalName.nonEmpty)
    require(absenceMessage.nonEmpty)

    def optional(parsed: Parsed[Seq[ParseTree]]): Parsed[Option[Seq[ParseTree]]] = {
      def adjust(done: Parsed[Seq[ParseTree]], currentPosition: Int): Parsed[Option[Seq[ParseTree]]] = {
        (done.map(some(_)).swap >>= (errors => {
          if (isErrorJustMeansWrongRuleApplication(currentPosition, errors.head.tokenIndex)) {
            EitherT.eitherT[ParserState, Option[Seq[ParseTree]], Seq[ParseError]](none[Seq[ParseTree]].left[Seq[ParseError]].point[ParserState])
          } else EitherT.eitherT[ParserState, Option[Seq[ParseTree]], Seq[ParseError]](errors.right[Option[Seq[ParseTree]]].point[ParserState])
        })).swap
      }

      for {
        (currentPosition, _) <- currentT
        adjusted <- adjust(parsed, currentPosition)
      } yield adjusted
    }

    def or(one: Parsed[Option[Seq[ParseTree]]], another: Parsed[Option[Seq[ParseTree]]]): Parsed[Option[Seq[ParseTree]]] = {
      one >>= (opt => {
        opt match {
          case Some(_) => EitherT.eitherT(opt.right[Seq[ParseError]].point[ParserState])
          case _ => another
        }
      })
    }

   val done: Parsed[Option[Seq[ParseTree]]] = others.foldLeft(optional(applyOrRollback(one)))((a, b) => or(a, optional(applyOrRollback(b))))
    done >>= {
      case Some(nodes) => attachNodesToNonTerminal(EitherT.eitherT(nodes.right.point[ParserState]), nonTerminalName)
      case _ => previousT >>= (p => EitherT.eitherT(Seq(ParseError(p._2.offset, p._1, absenceMessage)).left.point[ParserState]))
    }
  }

  def isErrorJustMeansWrongRuleApplication(currentPosition: Int, errorTokenPosition: Int) = errorTokenPosition <= currentPosition + 1

  def attachNodesToNonTerminal(parsed: Parsed[Seq[ParseTree]], nonTerminalName: String): Parsed[ParseTree] = {
    for {
      nodes <- parsed
      nonTerminal <- EitherT.eitherT(Tree.Node(nonTerminal(nonTerminalName), nodes.toStream).right[Seq[ParseError]].point[ParserState])
    } yield nonTerminal
  }

  implicit def seqMonoid[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def zero: Seq[T] = Seq.empty[T]
    override def append(f1: Seq[T], f2: => Seq[T]): Seq[T] = f1 ++: f2
  }

  implicit class ParsedTokenWrapper(parsed: Parsed[LexerToken]) {
    def toTree: Parsed[ParseTree] = {
      for {
        token <- parsed
        terminal <- EitherT.eitherT(terminal(token).leaf.right[Seq[ParseError]].point[ParserState])
      } yield terminal
    }
  }
}
