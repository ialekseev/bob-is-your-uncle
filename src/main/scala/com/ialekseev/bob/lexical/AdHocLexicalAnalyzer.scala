package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
class AdHocLexicalAnalyzer extends LexicalAnalyzer {

  case class Tokenized(token: Token, movePosition: Int)

  private object Identifier {
    def unapply(state: AdHocLexicalAnalysisState): Option[Tokenized] = {
      state.takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)).flatMap(t => token(state, identifier(t)))
    }
  }

  private object Variable {
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
        state.lookAhead(char => !isWS(char)).filter(c => !isNL(c._1)).map(nonWs => {
          val level = nonWs._2 - state.currentPos - 1
          Tokenized(Token.INDENT(state.currentPos, level), nonWs._2 - state.currentPos)
        })
      } else None
    }
  }

  private def token[T <: Token](state: AdHocLexicalAnalysisState, tokenWithoutPos: Option[Int => T]): Option[Tokenized] = {
    tokenWithoutPos.map(t => {
      val token = t(state.currentPos)
      Tokenized(token, token.length)
    })
  }

  private def addTokenAndMove(state: AdHocLexicalAnalysisState, tokenized: Tokenized) = {
    state.addToken(tokenized.token)
    state.move(tokenized.movePosition)
  }

  def tokenize(input: String): Either[List[LexicalAnalysisError], List[Token]] = {
    val state = new AdHocLexicalAnalysisState(input)

    while (state.hasNext) {
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
    if (errors.isEmpty)
      Right(state.extractResultingTokens)
    else
      Left(errors)
  }
}
