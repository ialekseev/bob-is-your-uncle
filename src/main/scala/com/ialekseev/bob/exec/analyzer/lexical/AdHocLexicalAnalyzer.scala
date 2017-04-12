package com.ialekseev.bob.exec.analyzer.lexical

import com.ialekseev.bob.LexicalAnalysisFailed
import com.ialekseev.bob.exec.analyzer.{LexerToken, Token, _}

import scalaz.Free.Trampoline
import scalaz.Scalaz._
import scalaz._

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
final class AdHocLexicalAnalyzer extends LexicalAnalyzer with LexicalAnalysisState {

  private def identifierStep: LexerState[Option[Tokenized]] = wordStep(currentIsId, takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)), identifier(_))
  private def variableStep: LexerState[Option[Tokenized]] = wordStep(currentIsVarFirst, takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)), variable(_))
  private def stringLiteralStep: LexerState[Option[Tokenized]] = wordStep(currentIsStringLiteralStart, takeAheadIncludingLast(isStringLiteralChar(_), isNL(_)), stringLiteral(_))
  private def dictionaryStep: LexerState[Option[Tokenized]] = wordStep(currentIsDictionaryStart, takeAheadIncludingLast(isDictionaryEndChar(_), isNL(_)), dictionary(_))
  private def jsonStep: LexerState[Option[Tokenized]] = wordStep(currentIsJsonStart, takeAheadIncludingLast(isJsonEndChar(_), isNL(_)), json(_))
  private def keywordStep: LexerState[Option[Tokenized]] = wordStep(currentIsId, takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)), keyword(_))

  private def blockStep: LexerState[Option[Tokenized]] = {
    currentIsId.ifM({
      get[LexerStateInternal] >>= (state => {
        (for {
          beginWord <- OptionT.optionT(takeAheadExcludingLast(isSeparator(_)))
          content <- OptionT.optionT(takeTillStr(state.position + beginWord.length, EOT.toString))
          blockToken <- OptionT.optionT(block(beginWord, content).point[LexerState])
        } yield Tokenized(blockToken, state.position, beginWord.length + content.length)).run
      })
    },none[Tokenized].point[LexerState])
  }

  private def delimiterStep: LexerState[Option[Tokenized]] = currentChar >>= (c => token(delimiter(c)))

  private def indentStep: LexerState[Option[Tokenized]] = {
    currentIsNL.ifM({
      get[LexerStateInternal] >>= (state => {
        (for {
          ahead: (Char, Int) <- OptionT.optionT(lookAhead(char => !isWS(char) && !isNL(char)))
          nl <- OptionT.optionT(lookBack(isNL(_), ahead._2))
        } yield {
            val indentLevel = ahead._2 - nl._2 - 1
            Tokenized(Token.INDENT(indentLevel), nl._2 + 1, ahead._2 - state.position)
          }).run
      })
    }, none[Tokenized].point[LexerState])
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

  def tokenize(source: String): LexicalAnalysisFailed \/ List[LexerToken] = {
    if (source.nonEmpty) {
      type LexerStateT[S] = StateT[Trampoline, LexerStateInternal, S]

      def go(state: LexerStateT[Unit]): LexerStateT[Unit] = {
        state >> {
          val steps = (for {
            _ <- OptionT.optionT(addTokenAndMove(blockStep))
            _ <- OptionT.optionT(addTokenAndMove(keywordStep))
            _ <- OptionT.optionT(addTokenAndMove(variableStep))
            _ <- OptionT.optionT(addTokenAndMove(delimiterStep))
            _ <- OptionT.optionT(addTokenAndMove(stringLiteralStep))
            _ <- OptionT.optionT(addTokenAndMove(dictionaryStep))
            _ <- OptionT.optionT(addTokenAndMove(jsonStep))
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
        val extractResult: LexerState[LexicalAnalysisFailed \/ Vector[LexerToken]] = extractErrors >>= (errors => {
          if (errors.isEmpty) extractResultingTokens.map(_.right)
          else LexicalAnalysisFailed(errors.toList).left.point[LexerState]
        })
        extractResult.lift[Trampoline]
      }

      (goT >> extractResultT).run(LexerStateInternal(source, 0, Vector.empty, Vector.empty)).run._2.map(_.toList)

    } else Nil.right
  }
}
