package com.ialekseev.bob.lexical

import com.ialekseev.bob._
import scala.annotation.tailrec
import scalaz.Free.Trampoline
import scalaz._
import scalaz.Scalaz._
import scalaz.Free._

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
final class AdHocLexicalAnalyzer extends LexicalAnalyzer with AdHocLexicalAnalysis {

  protected case class Tokenized(token: Token, offset: Int, movePosition: Int) {
    require(movePosition > 0)
  }

  private def identifierStep: LexerState[Option[Tokenized]] = {
    currentIsId >>= (isId => {
      if (isId) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead >>= (identifier(_)))
        } yield token
      }
      else none[Tokenized].point[LexerState]
    })
  }

  private def variableStep: LexerState[Option[Tokenized]] = {
    currentIsVariableStart >>= (isVariableStart => {
      if (isVariableStart) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead >>= (variable(_)))
        } yield token
      }
      else none[Tokenized].point[LexerState]
    })
  }

  private def stringLiteralStep: LexerState[Option[Tokenized]] = {
    currentIsStringLiteralStart >>= (isStringLiteralStart => {
      for {
        ahead <- takeAheadIncludingLast(isStringLiteralChar(_), isNL(_))
        token <- token(ahead >>= (stringLiteral(_)))
      } yield token
    })
  }

  private def keywordStep: LexerState[Option[Tokenized]] = {
    currentIsId >>= (isId => {
      if (isId) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead >>= (keyword(_)))
        } yield token
      }
      else none[Tokenized].point[LexerState]
    })
  }

  private def delimiterStep: LexerState[Option[Tokenized]] = {
    currentChar >>= (c => token(delimiter(c)))
  }

  private def indentStep: LexerState[Option[Tokenized]] = {
    currentIsNL >>= (currentIsNL => {
      if (currentIsNL) {
        get[LexerStateInternal] >>= (state => {
          (for {
            ahead: (Char, Int) <- OptionT.optionT(lookAhead(char => !isWS(char) && !isNL(char)))
            nl <- OptionT.optionT(lookBack(isNL(_), ahead._2))
          } yield {
            val indentLevel = ahead._2 - nl._2 - 1
            Tokenized(Token.INDENT(indentLevel), nl._2 + 1, ahead._2 - state.position)
          }).run
        })
      }
      else none[Tokenized].point[LexerState]
    })
  }

  private def token(token: Option[Token]): LexerState[Option[Tokenized]] = {
    get.map(s => token.map(t => Tokenized(t, s.position, t.length)))
  }

  private def addTokenAndMove(step: LexerState[Option[Tokenized]]): LexerState[Option[Unit]] = {
    val addAndMove: LexerState[Option[Tokenized]] = step >>= {
      case t@Some(tokenized) => (addToken(tokenized.token, tokenized.offset) >> move(tokenized.movePosition)) >| t
      case None => step
    }
    addAndMove.map(v => (if (v.isDefined) None else someUnit))
  }

  private def ignoreWhiteSpaceAndMove(): LexerState[Option[Unit]] = {
    val isWSorNL = (currentIsWS |@| currentIsNL)(_ || _)

    isWSorNL >>= (is => {
      if (is) moveNext >| none[Unit]
      else someUnit.point[LexerState]
    })
  }

  private def addErrorOffsetAndMove(): LexerState[Option[Unit]] = {
    val add: LexerState[Unit] = (get[LexerStateInternal]) >>= (s => addErrorOffset(s.position))
    (add >> moveNext) >| someUnit
  }

  def tokenize(input: String): \/[Seq[LexerError], Seq[LexerToken]] = {
    if (input.nonEmpty) {
      type LexerStateT[S] = StateT[Trampoline, LexerStateInternal, S]

      def go(state: LexerStateT[Unit]): LexerStateT[Unit] = {
        state >> {
          val steps = (for {
            _ <- OptionT.optionT(addTokenAndMove(keywordStep))
            _ <- OptionT.optionT(addTokenAndMove(variableStep))
            _ <- OptionT.optionT(addTokenAndMove(delimiterStep))
            _ <- OptionT.optionT(addTokenAndMove(stringLiteralStep))
            _ <- OptionT.optionT(addTokenAndMove(identifierStep))
            _ <- OptionT.optionT(addTokenAndMove(indentStep))
            _ <- OptionT.optionT(ignoreWhiteSpaceAndMove)
            _ <- OptionT.optionT(addErrorOffsetAndMove)
          } yield ()).run

          (steps >| unit).lift[Trampoline]
        }
      }

      val goT = {
        val initialT = (get[LexerStateInternal] >| unit).lift[Trampoline]
        val goWhileT = hasCurrent.lift[Trampoline]
        go(initialT).whileM_(goWhileT)
      }

      val extractResultT = {
        val extractResult: LexerState[\/[Seq[LexerError], Seq[LexerToken]]] = extractErrors >>= (errors => {
          if (errors.isEmpty) extractResultingTokens.map(_.right)
          else errors.left.point[LexerState]
        })
        extractResult.lift[Trampoline]
      }

      (goT >> extractResultT).run(LexerStateInternal(input, 0, Nil, Nil)).run._2

    } else Nil.right
  }
}
