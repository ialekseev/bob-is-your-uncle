package com.ialekseev.bob.analyzer.syntax

import com.ialekseev.bob.SyntaxError
import com.ialekseev.bob.analyzer.syntax.SyntaxAnalyzer._
import com.ialekseev.bob.analyzer.{LexerToken, Token, TokenTag}
import scala.reflect.ClassTag
import scalaz.Scalaz._
import scalaz._

private[syntax] trait SyntaxAnalysisState {
  type Parsed[R] = EitherT[ParserState, List[SyntaxError], R]
  type ParserState[S] = State[ParserStateInternal, S]
  type IndentLevel = Int
  type IndentLength = Int

  case class ParserStateInternal(val tokens: Seq[LexerToken], position: Int, indentMap: Map[IndentLevel, IndentLength])

  def current: ParserState[(Int, Option[LexerToken])] = get.map(s => {
    if (s.position < s.tokens.length) (s.position, some(s.tokens(s.position)))
    else (s.position, none)
  })
  def currentT: Parsed[(Int, Option[LexerToken])] = EitherT.eitherT[ParserState, List[SyntaxError], (Int, Option[LexerToken])](current.map(_.right[List[SyntaxError]]))

  def previous: ParserState[(Int, LexerToken)] = get.map(s => (s.position - 1, s.tokens(s.position - 1)))
  def previousT: Parsed[(Int, LexerToken)] = EitherT.eitherT[ParserState, List[SyntaxError], (Int, LexerToken)](previous.map(_.right[List[SyntaxError]]))

  def move: ParserState[Unit] = modify(s => s.copy(position = s.position + 1))
  def jump(to: Int): ParserState[Unit] = modify(s => s.copy(position = to))

  def parseToken[T <: Token : ClassTag](implicit tokenShow: Show[Token], tokenTag: TokenTag[T]): Parsed[LexerToken]= {
    EitherT.eitherT {
      current >>= {
        case (_, Some(t@LexerToken(_: T, _))) => move >| t.right
        case (position, Some(t@LexerToken(token, _))) => List(SyntaxError(t.startOffset, t.endOffset, position, s"Unexpected: '${tokenShow.show(token)}' (expecting: '${tokenTag.asString}')")).left.point[ParserState]
        case (_, None) => get[ParserStateInternal].map(s => List(SyntaxError(s.tokens.last.startOffset, s.tokens.last.endOffset, s.position, "Unexpected end")).left)
      }
    }
  }

  def parse[T <: Token: ClassTag](implicit tokenShow: Show[Token], tokenTag: TokenTag[T]): Parsed[ParseTree] = parseToken[T].toTree
  def parse[T <: Token.INDENT](indentLevel: IndentLevel): Parsed[ParseTree] = {
    require(indentLevel >= 0)

    (for {
      indent <- parseToken[Token.INDENT]
      result: LexerToken <- EitherT.eitherT[ParserState, List[SyntaxError], LexerToken] {
        get[ParserStateInternal] >>= (state => {
          if (state.indentMap.contains(indentLevel) && state.indentMap(indentLevel) === indent.token.length){
            get[ParserStateInternal] >| indent.right
            indent.right.point[ParserState]
          } else if (!state.indentMap.contains(indentLevel) && (!state.indentMap.contains(indentLevel - 1) || (state.indentMap(indentLevel - 1) < indent.token.length))) {
            modify[ParserStateInternal](s => s.copy(indentMap = s.indentMap + (indentLevel -> indent.token.length))) >>
              indent.right.point[ParserState]
          } else {
            val expectedIndentWidthMessage = (state.indentMap.get(indentLevel).map(_.toString).orElse(state.indentMap.get(indentLevel - 1).map("> " + _))).map("Expected: " + _ ).getOrElse("")
            List(SyntaxError(indent.startOffset, indent.endOffset, state.position - 1, s"Unexpected indent width: ${indent.token.length}. $expectedIndentWidthMessage")).left.point[ParserState]
          }
        })
      }
    } yield result).toTree
  }

  def applyOrRollback(apply: Parsed[List[ParseTree]]): Parsed[List[ParseTree]]  = {
    for {
      (position, _) <- currentT
      applied <- (apply.swap >>= (error => EitherT.eitherT(jump(position) >| error.right[List[ParseTree]]))).swap
    } yield applied
  }

  def rule(nonTerminalName: String)(apply:  Parsed[List[ParseTree]]): Parsed[ParseTree] = {
    require(nonTerminalName.nonEmpty)

    attachNodesToNonTerminal(applyOrRollback(apply), nonTerminalName)
  }

  //todo: trampoline?
  def repeat(nonTerminalName: String)(parsed: => Parsed[ParseTree]): Parsed[Option[ParseTree]] = {
    require(nonTerminalName.nonEmpty)

    def go(nodes: List[ParseTree]): EitherT[ParserState, (SyntaxError, List[ParseTree]), List[ParseTree]] = {
      for {
        node <- parsed.leftMap(errors => (errors.head, nodes))
        result <- go(nodes :+ node)
      } yield result
    }

    for {
      done: (SyntaxError, List[ParseTree]) <- go(List.empty[ParseTree]).swap.leftMap(_ => List.empty[SyntaxError])
      (currentPosition, _) <- currentT
      result: Option[ParseTree] <- {
        if (isErrorJustMeansWrongRuleApplication(currentPosition, done._1.tokenIndex)) {
          done._2 match {
            case Seq() => EitherT.eitherT(none.right[List[SyntaxError]].point[ParserState])
            case nodes@Seq(_, _*) => attachNodesToNonTerminal(EitherT.eitherT(done._2.right.point[ParserState]), nonTerminalName).map(some(_))
          }
        }
        else EitherT.eitherT(List(done._1).left[Option[ParseTree]].point[ParserState])
      }
    } yield result
  }

  def or(nonTerminalName: String)(absenceMessage: String)(one: Parsed[List[ParseTree]], others: Parsed[List[ParseTree]]*): Parsed[ParseTree] = {
    require(nonTerminalName.nonEmpty)
    require(absenceMessage.nonEmpty)

    def optional(parsed: Parsed[List[ParseTree]]): Parsed[Option[List[ParseTree]]] = {
      def adjust(done: Parsed[List[ParseTree]], currentPosition: Int): Parsed[Option[List[ParseTree]]] = {
        (done.map(some(_)).swap >>= (errors => {
          if (isErrorJustMeansWrongRuleApplication(currentPosition, errors.head.tokenIndex)) {
            EitherT.eitherT[ParserState, Option[List[ParseTree]], List[SyntaxError]](none[List[ParseTree]].left[List[SyntaxError]].point[ParserState])
          } else EitherT.eitherT[ParserState, Option[List[ParseTree]], List[SyntaxError]](errors.right[Option[List[ParseTree]]].point[ParserState])
        })).swap
      }

      for {
        (currentPosition, _) <- currentT
        adjusted <- adjust(parsed, currentPosition)
      } yield adjusted
    }

    def or(one: Parsed[Option[List[ParseTree]]], another: Parsed[Option[List[ParseTree]]]): Parsed[Option[List[ParseTree]]] = {
      one >>= (opt => {
        opt match {
          case Some(_) => EitherT.eitherT(opt.right[List[SyntaxError]].point[ParserState])
          case _ => another
        }
      })
    }

   val done: Parsed[Option[List[ParseTree]]] = others.foldLeft(optional(applyOrRollback(one)))((a, b) => or(a, optional(applyOrRollback(b))))
    done >>= {
      case Some(nodes) => attachNodesToNonTerminal(EitherT.eitherT(nodes.right.point[ParserState]), nonTerminalName)
      case _ => EitherT.eitherT[ParserState, List[SyntaxError], ParseTree](get[ParserStateInternal] >>= (s => {
        val position = if (s.position < s.tokens.length) s.position else s.position - 1
        val token = s.tokens(position)
        List(SyntaxError(token.startOffset, token.endOffset, position, absenceMessage)).left.point[ParserState]
      }))
    }
  }

  def isErrorJustMeansWrongRuleApplication(currentPosition: Int, errorTokenPosition: Int) = errorTokenPosition <= currentPosition + 1

  def attachNodesToNonTerminal(parsed: Parsed[List[ParseTree]], nonTerminalName: String): Parsed[ParseTree] = {
    for {
      nodes <- parsed
      nonTerminal <- EitherT.eitherT(Tree.Node(nonTerminal(nonTerminalName), nodes.toStream).right[List[SyntaxError]].point[ParserState])
    } yield nonTerminal
  }

  implicit class ParsedTokenWrapper(parsed: Parsed[LexerToken]) {
    def toTree: Parsed[ParseTree] = {
      for {
        token <- parsed
        terminal <- EitherT.eitherT(terminal(token).leaf.right[List[SyntaxError]].point[ParserState])
      } yield terminal
    }
  }
}
