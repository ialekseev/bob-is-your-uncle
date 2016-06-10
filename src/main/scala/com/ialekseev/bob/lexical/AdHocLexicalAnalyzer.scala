package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token

//Simple Ad hoc lexical analyzer without Regular Expressions and Finite Automata
class AdHocLexicalAnalyzer extends LexicalAnalyzer {

  case class Tokenized(token: Token, movePosition: Int)

  private object Identifier {
    def unapply(state: LexicalAnalysisState): Option[Tokenized] = {
      state.takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)).flatMap(t => token(state, identifier(t)))
    }
  }

  private object Variable {
    def unapply(state: LexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsVariableStart)
        state.takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)).flatMap(t => token(state, variable(t)))
      else None
    }
  }

  private object StringLiteral {
    def unapply(state: LexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsStringLiteralStart)
        state.takeAheadIncludingLast(isStringLiteralChar(_), isNL(_)).flatMap(t => token(state, stringLiteral(t)))
      else None
    }
  }

  private object Keyword {
    def unapply(state: LexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsId)
        state.takeAheadExcludingLast(isSeparator(_), isStringLiteralChar(_)).flatMap(t => token(state, keyword(t)))
      else None
    }
  }

  private object Delimiter {
    def unapply(state: LexicalAnalysisState): Option[Tokenized] = {
      token(state, delimiter(state.currentChar))
    }
  }

  private object NL {
    def unapply(state: LexicalAnalysisState): Option[Tokenized] = {
      if (state.currentIsNL){
        state.lookAhead(char => !isNL(char) && !isWS(char)).map(next => {
          Tokenized(Token.NL(state.currentPos), next._2 - state.currentPos)
        })
      } else None
    }
  }

  private def token[T <: Token](state: LexicalAnalysisState, tokenWithoutPos: Option[Int => T]): Option[Tokenized] = {
    tokenWithoutPos.map(t => {
      val token = t(state.currentPos)
      Tokenized(token, token.length)
    })
  }

  private def addTokenAndMove(state: LexicalAnalysisState, tokenized: Tokenized) = {
    state.addToken(tokenized.token)
    state.move(tokenized.movePosition)
  }

  private def addErrorAndMove(state: LexicalAnalysisState)(error: LexicalAnalysisError) = {
    state.addError(error)
    state.moveNext
  }

  def tokenize(input: String): Either[List[LexicalAnalysisError], List[Token]] = {
    require(!input.isEmpty)

    val state = new LexicalAnalysisState(input)

    //todo: ignore WS
    while (state.hasNext) {
      state match {
        case Keyword(tokenized) => addTokenAndMove(state, tokenized)
        case Variable(tokenized) => addTokenAndMove(state, tokenized)
        case Delimiter(tokenized) => addTokenAndMove(state, tokenized)
        case StringLiteral(tokenized) => addTokenAndMove(state, tokenized)
        case Identifier(tokenized) => addTokenAndMove(state, tokenized)
        case NL(tokenized) => addTokenAndMove(state, tokenized)
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
