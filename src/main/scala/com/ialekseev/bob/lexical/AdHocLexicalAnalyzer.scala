package com.ialekseev.bob.lexical

import com.ialekseev.bob._
import scala.annotation.tailrec
import scalaz.Free.Trampoline
import scalaz._
import scalaz.Scalaz._

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
final class AdHocLexicalAnalyzer extends LexicalAnalyzer with LexicalAnalysisState {

  private def identifierStep: LexerState[Option[Tokenized]] = wordStep(currentIsId, takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)), identifier(_))
  private def variableStep: LexerState[Option[Tokenized]] = wordStep(currentIsVariableStart, takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)), variable(_))
  private def stringLiteralStep: LexerState[Option[Tokenized]] = wordStep(currentIsStringLiteralStart, takeAheadIncludingLast(isStringLiteralChar(_), isNL(_)), stringLiteral(_))
  private def dictionaryStep: LexerState[Option[Tokenized]] = wordStep(currentIsDictionaryStart, takeAheadIncludingLast(isDictionaryEndChar(_), isNL(_)), dictionary(_))
  private def keywordStep: LexerState[Option[Tokenized]] = wordStep(currentIsId, takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)), keyword(_))
  private def delimiterStep: LexerState[Option[Tokenized]] = currentChar >>= (c => token(delimiter(c)))

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

  private def ignoreWhiteSpaceAndMove(): LexerState[Option[Unit]] = {
    val isWSorNL = (currentIsWS |@| currentIsNL)(_ || _)

    isWSorNL >>= (is => {
      if (is) moveNext >| none[Unit]
      else someUnit.point[LexerState]
    })
  }

  private def addErrorAndMove(): LexerState[Option[Unit]] = {
    val posAndSeparator: LexerState[(Int, Option[(Char, Int)])] = (currentPos |@| lookAhead(isSeparator(_)))((_,_))
    (posAndSeparator >>= {
      case (pos, Some(sep)) => addError(pos, sep._2 - 1) >> jump(sep._2)
      case (pos, None) => addError(pos, pos) >> moveNext
    }) >| someUnit
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
            _ <- OptionT.optionT(addTokenAndMove(dictionaryStep))
            _ <- OptionT.optionT(addTokenAndMove(identifierStep))
            _ <- OptionT.optionT(addTokenAndMove(indentStep))
            _ <- OptionT.optionT(ignoreWhiteSpaceAndMove)
            _ <- OptionT.optionT(addErrorAndMove)
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
