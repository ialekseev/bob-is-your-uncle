package com.ialekseev.bob.lexical

import com.ialekseev.bob._
import scala.annotation.tailrec
import scalaz.Free.Trampoline
import scalaz._
import scalaz.Scalaz._
import scalaz.Free._

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
final class AdHocLexicalAnalyzer extends LexicalAnalyzer with AdHocLexicalAnalysis {

  protected case class Tokenized(token: Token, movePosition: Int) {
    require(movePosition > 0)
  }

  private def identifierStep: LexerState[Option[Tokenized]] = {
    currentIsId.flatMap(isId => {
      val test = isId
      if (isId) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead.flatMap(a => identifier(a)))
        } yield token
      }
      else none[Tokenized].point[LexerState]
    })
  }

  private def variableStep: LexerState[Option[Tokenized]] = {
    currentIsVariableStart.flatMap(isVariableStart => {
      if (isVariableStart) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead.flatMap(a => variable(a)))
        } yield token
      }
      else none[Tokenized].point[LexerState]
    })
  }

  private def stringLiteralStep: LexerState[Option[Tokenized]] = {
    currentIsStringLiteralStart.flatMap(isStringLiteralStart => {
      for {
        ahead <- takeAheadIncludingLast(isStringLiteralChar(_), isNL(_))
        token <- token(ahead.flatMap(a => stringLiteral(a)))
      } yield token
    })
  }

  private def keywordStep: LexerState[Option[Tokenized]] = {
    currentIsId.flatMap(isId => {
      if (isId) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead.flatMap(a => keyword(a)))
        } yield token
      }
      else none[Tokenized].point[LexerState]
    })
  }

  private def delimiterStep: LexerState[Option[Tokenized]] = {
    currentChar.flatMap(c => token(delimiter(c)))
  }

  private def indentStep: LexerState[Option[Tokenized]] = {
    currentIsNL.flatMap(currentIsNL => {
      if (currentIsNL) {
        get.flatMap(state => {
          (for {
            nonWs: (Char, Int) <- OptionT.optionT(lookAhead(char => !isWS(char)))
            nonNl <- OptionT.optionT((if (!isNL(nonWs._1)) some(nonWs) else none).point[LexerState])
            nl <- OptionT.optionT(lookBack(isNL(_)))
          } yield {
            val level = nonWs._2 - nl._2 - 1
            Tokenized(Token.INDENT(state.position, level), nonWs._2 - state.position)
          }).run
        })
      }
      else none[Tokenized].point[LexerState]
    })
  }

  private def token[T <: Token](freeToken: Option[FreeToken]): LexerState[Option[Tokenized]] = {
    get.map(s => {
      freeToken.map(t => {
        val token = t(s.position)
        Tokenized(token, token.length)
      })
    })
  }

  private def addTokenAndMove(step: LexerState[Option[Tokenized]]): LexerState[Option[Unit]] = {
    val addAndMove: LexerState[Option[Tokenized]] = step.flatMap {
      case t@Some(tokenized) => addToken(tokenized.token).flatMap(_ => move(tokenized.movePosition).map(_ => t))
      case None => step
    }
    addAndMove.map(v => (if (v.isDefined) None else Some((): Unit)))
  }

  private def ignoreWhiteSpaceAndMove(): LexerState[Option[Unit]] = {
    val isWSorNL = for {
      isWS <- currentIsWS
      isNL <- currentIsNL
      isWSorNL <- (isWS || isNL).point[LexerState]
    } yield isWSorNL

    isWSorNL.flatMap(is => {
      if (is) moveNext.map(_ => none[Unit])
      else some(():Unit).point[LexerState]
    })
  }

  private def addErrorOffsetAndMove(): LexerState[Option[Unit]] = {
    for {
      state <- get[LexerStateInternal]
      modify <- addErrorOffset(state.position)
      move <- moveNext
    } yield some((): Unit)
  }

  def tokenize(input: String): Either[List[LexicalAnalysisError], List[Token]] = {
    if (input.nonEmpty) {
      type LexerStateT[S] = StateT[Trampoline, LexerStateInternal, S]

      def go(state: LexerStateT[Unit]): LexerStateT[Unit] = {
        state.flatMap(_ => {
          (for {
            _ <- OptionT.optionT(addTokenAndMove(keywordStep))
            _ <- OptionT.optionT(addTokenAndMove(variableStep))
            _ <- OptionT.optionT(addTokenAndMove(delimiterStep))
            _ <- OptionT.optionT(addTokenAndMove(stringLiteralStep))
            _ <- OptionT.optionT(addTokenAndMove(identifierStep))
            _ <- OptionT.optionT(addTokenAndMove(indentStep))
            _ <- OptionT.optionT(ignoreWhiteSpaceAndMove)
            _ <- OptionT.optionT(addErrorOffsetAndMove)
          } yield ()).run.map(_ => (): Unit).lift[Trampoline]
        })
      }

      val tokenizer =  for {
        result <- go(get[LexerStateInternal].map(_ => ()).lift[Trampoline]).whileM_(hasCurrent.lift[Trampoline])
        errors <- extractErrors.lift[Trampoline]
        tokens <- extractResultingTokens.lift[Trampoline]
      } yield if (errors.isEmpty) Right(tokens) else Left(errors)

      tokenizer.run(LexerStateInternal(input, 0, Nil, Nil)).run._2
    } else Right(Nil)
  }
}
