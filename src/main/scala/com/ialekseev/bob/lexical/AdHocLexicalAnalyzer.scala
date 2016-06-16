package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token
import scala.annotation.tailrec
import scalaz._
import Scalaz._
import com.ialekseev.bob.lexical.AdHocLexicalAnalysisState._

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
class AdHocLexicalAnalyzer extends LexicalAnalyzer with AdHocLexicalAnalysis {

  case class Tokenized(token: Token, movePosition: Int)

  def identifierStep: LexerState[Option[Tokenized]] = {
    currentIsId.flatMap(isId => {
      if (isId) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead.flatMap(a => identifier(a)))
        } yield token
      }
      else get.map(_=> None)
    })
  }

  def variableStep: LexerState[Option[Tokenized]] = {
    currentIsVariableStart.flatMap(isVariableStart => {
      if (isVariableStart) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead.flatMap(a => variable(a)))
        } yield token
      }
      else get.map(_=> None)
    })
  }

  def stringLiteralStep: LexerState[Option[Tokenized]] = {
    currentIsStringLiteralStart.flatMap(isStringLiteralStart => {
      for {
        ahead <- takeAheadIncludingLast(isStringLiteralChar(_), isNL(_))
        token <- token(ahead.flatMap(a => stringLiteral(a)))
      } yield token
    })
  }

  def keywordStep: LexerState[Option[Tokenized]] = {
    currentIsId.flatMap(isId => {
      if (isId) {
        for {
          ahead <- takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_))
          token <- token(ahead.flatMap(a => keyword(a)))
        } yield token
      }
      else get.map(_=> None)
    })
  }

  def delimiterStep: LexerState[Option[Tokenized]] = {
    currentChar.flatMap(c => token(delimiter(c)))
  }

  //todo
  /*def IndentStep: LexerState[Option[Tokenized]] = {
    currentIsNL.flatMap(isNL => {
      for {
        nonWs <- lookAhead(char => !isWS(char)).filter(c => !isNL(c._1))
        nl <- lookBack(isNL(_))
      } yield {
        val level = nonWs._2 - nl._2 - 1
        Tokenized(Token.INDENT(state.currentPos, level), nonWs._2 - state.currentPos)
      }
    })
  }*/

  /*private object INDENT {
    def unapply(state: AdHocLexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsNL){
        state.lookAhead(char => !isWS(char)).filter(c => !isNL(c._1)).flatMap(nonWs => {
          state.lookBack(isNL(_)).map(nl => {
            val level = nonWs._2 - nl._2 - 1
            Tokenized(Token.INDENT(state.currentPos, level), nonWs._2 - state.currentPos)
          })
        })
      } else None
    }
  }*/

  /*private object Variable {
    def unapply(state: AdHocLexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsVariableStart)
        state.takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)).flatMap(t => token(state, variable(t)))
      else None
    }
  }

  private object StringLiteral {
    def unapply(state: AdHocLexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsStringLiteralStart)
        state.takeAheadIncludingLast(isStringLiteralChar(_), isNL(_)).flatMap(t => token(state, stringLiteral(t)))
      else None
    }
  }

  private object Keyword {
    def unapply(state: AdHocLexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsId)
        state.takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)).flatMap(t => token(state, keyword(t)))
      else None
    }
  }

  private object Delimiter {
    def unapply(state: AdHocLexicalAnalysisState): Option[Tokenized] = {
      token(state, delimiter(state.currentChar))
    }
  }

  private object INDENT {
    def unapply(state: AdHocLexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsNL){
        state.lookAhead(char => !isWS(char)).filter(c => !isNL(c._1)).flatMap(nonWs => {
          state.lookBack(isNL(_)).map(nl => {
            val level = nonWs._2 - nl._2 - 1
            Tokenized(Token.INDENT(state.currentPos, level), nonWs._2 - state.currentPos)
          })
        })
      } else None
    }
  }*/

  /*private def token[T <: Token](tokenWithoutPos: Option[Int => T]): LexerState[Option[Tokenized]] = {
    get.map(s => {
      tokenWithoutPos.map(t => {
        val token = t(s.position)
        Tokenized(token, token.length)
      })
    })
  }*/

  /*private def addTokenAndMove(state: AdHocLexicalAnalysisState, tokenized: Tokenized) = {
    state.addToken(tokenized.token)
    state.move(tokenized.movePosition)
  }*/

  private def token[T <: Token](tokenWithoutPos: Option[Int => T]): LexerState[Option[Tokenized]] = {
    get.map(s => {
      tokenWithoutPos.map(t => {
        val token = t(s.position)
        Tokenized(token, token.length)
      })
    })
  }

  def addTokenAndMove(step: LexerState[Option[Tokenized]]): LexerState[Option[Unit]] = {
    val addAndMove: LexerState[Option[Tokenized]] = step.flatMap {
      case t@Some(tokenized) => addToken(tokenized.token).flatMap(_ => move(tokenized.movePosition).map(_ => t))
      case None => step
    }
    addAndMove.map(v => (if (v.isDefined) None else Some((): Unit)))
  }

  def ignoreWhiteSpaceStep: LexerState[Option[Unit]] = {
    currentIsWS.flatMap(isWS => currentIsNL.map(isNL => (isWS || isNL))).flatMap {
      case true => moveNext.map(_ => None)
      case false => get.map(_ => Some((): Unit))
    }
  }

  def errorStep: LexerState[Option[Unit]] = {
    get[LexerStateInternal].flatMap(s => addErrorOffset(s.position)).map(_ => Some((): Unit))
  }

  //todo: use trampoline
  def tokenize(input: String): Either[List[LexicalAnalysisError], List[Token]] = {
    if (input.nonEmpty) {
      def go(state: LexerState[Unit]) = {
        state.flatMap(_ => {
          (for {
            _ <- OptionT.optionT(addTokenAndMove(keywordStep))
            _ <- OptionT.optionT(addTokenAndMove(variableStep))
            _ <- OptionT.optionT(addTokenAndMove(delimiterStep))
            _ <- OptionT.optionT(addTokenAndMove(stringLiteralStep))
            _ <- OptionT.optionT(addTokenAndMove(identifierStep))
            _ <- OptionT.optionT(ignoreWhiteSpaceStep)
            _ <- OptionT.optionT(errorStep)
          } yield ()).getOrElse((): Unit)
        })
      }

      val tokenizer =  for {
        result <- go(AdHocLexicalAnalysisState()).whileM_(hasCurrent)
        errors <- extractErrors
        tokens <- extractResultingTokens
      } yield if (errors.isEmpty) Right(tokens) else Left(errors)

      tokenizer.run(LexerStateInternal(input, 0, Nil, Nil))._2
    } else Right(Nil)
  }
}
