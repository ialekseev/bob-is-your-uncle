package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token
import scala.annotation.tailrec
import scalaz._
import Scalaz._
import com.ialekseev.bob.lexical.AdHocLexicalAnalysisState._

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
class AdHocLexicalAnalyzer extends LexicalAnalyzer with AdHocLexicalAnalysis {

  case class Tokenized(token: Token, movePosition: Int)


  def checkIdentifier: LexerState[Option[Tokenized]] = {
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

  def tokenize(input: String): Either[List[LexicalAnalysisError], List[Token]] = {

    def loop(state: LexerState[Unit]): LexerState[Unit] = {
      /*def check(c: LexerState[Option[Tokenized]]) = c.flatMap {
          case Some(tokenized) => loop(move(tokenized.movePosition))
          case None => state
      }*/

        state.flatMap(_ => {
            hasCurrent.flatMap(has => {
              if (has) {
                checkIdentifier.flatMap {
                  case Some(tokenized) => loop(addToken(tokenized.token).flatMap(_ => move(tokenized.movePosition)))
                  case None => currentIsWS.flatMap(isWS => currentIsNL.map(isNL => (isWS || isNL))).flatMap {
                    case true =>  loop(moveNext)
                    case false =>  get[LexerStateInternal].flatMap(s => addErrorOffset(s.position))
                  }
                }
              }
              else state
            })
        })
    }

    val tokenizer =  for {
      result <- loop(AdHocLexicalAnalysisState())
      errors <- extractErrors
      tokens <- extractResultingTokens
    } yield if (errors.isEmpty) Right(tokens) else Left(errors)

    tokenizer.run(LexerStateInternal(input, 0, Nil, Nil))._2


    //val state = loop(AdHocLexicalAnalysisState(input))

    /*if (input.nonEmpty) {
      val state = AdHocLexicalAnalysisState(input)

      while (state.hasCurrent) {
        state match {
          case Keyword(tokenized) => addTokenAndMove(state, tokenized)
          case Variable(tokenized) => addTokenAndMove(state, tokenized)
          case Delimiter(tokenized) => addTokenAndMove(state, tokenized)
          case StringLiteral(tokenized) => addTokenAndMove(state, tokenized)
          case Identifier(tokenized) => addTokenAndMove(state, tokenized)
          case INDENT(tokenized) => addTokenAndMove(state, tokenized)
          case _ if isWS(state.currentChar) || isNL(state.currentChar) => state.moveNext
          case s => {
            state.addErrorOffset(state.currentPos)
            state.moveNext
          }
        }
      }

      val errors = state.extractErrors
      if (errors.isEmpty) Right(state.extractResultingTokens)
      else Left(errors)

    } else Right(Nil)*/
  }
}
