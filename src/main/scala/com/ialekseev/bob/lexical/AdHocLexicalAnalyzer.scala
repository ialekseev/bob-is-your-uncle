package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token
import com.ialekseev.bob.Token.Delimiter.DelimiterToken
import com.ialekseev.bob.Token.Keyword.KeywordToken
import com.ialekseev.bob.Token.Variable

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
class AdHocLexicalAnalyzer extends LexicalAnalyzer {

  private object Variable {
    def unapply(state: LexicalAnalysisState): Option[Variable] = {
      if (state.currentIsVariableStart) token(state)(variable(state.takeStringTillSeparator)) else None
    }
  }

  private object Keyword {
    def unapply(state: LexicalAnalysisState): Option[KeywordToken] = {
      if (state.currentIsId) token(state)(keyword(state.takeStringTillSeparator)) else None
    }
  }

  private object Delimiter {
    def unapply(state: LexicalAnalysisState): Option[DelimiterToken] = {
      token(state)(delimiter(state.currentChar))
    }
  }

  private def token[T <: Token](state: LexicalAnalysisState)(tokenWithoutPos: Option[Int => T]) = {
    tokenWithoutPos.map(_(state.currentPos))
  }

  private def addTokenAndMove(state: LexicalAnalysisState)(token: Token) = {
    state.addToken(token)
    state.move(token.length)
  }

  private def addErrorAndMove(state: LexicalAnalysisState)(error: LexicalAnalysisError) = {
    state.addError(error)
    state.moveNext
  }

  def tokenize(input: String): Either[List[LexicalAnalysisError], List[Token]] = {
    require(!input.isEmpty)

    val state = new LexicalAnalysisState(input)

    while (state.hasNext) {
      state match {
        case Keyword(token) => addTokenAndMove(state)(token)
        case Variable(token) => addTokenAndMove(state)(token)
        case Delimiter(token) => addTokenAndMove(state)(token)
        case s => addErrorAndMove(state)(LexicalAnalysisError(s.currentPos, s"Unexpected char: '${s.currentChar}'"))
      }
    }

    val errors = state.extractErrors
    if (errors.isEmpty)
      Right(state.extractResultingTokens)
    else
      Left(errors)
  }
}
