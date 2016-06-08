package com.ialekseev.bob

import scala.collection.mutable.ArrayBuffer

/*[Lexical Syntax in EBNF form]
      upper ::=  'A' |...| 'Z'
      lower ::=  'a' |...| 'z'
      letter ::= upper | lower
      digit ::=  '0' |...| '9'
      id ::= letter | digit

      identifier ::= id {id}
      variable ::= '$' identifier
      stringLiteral ::= "..."
      keyword ::= 'namespace' | 'description' | 'get' | 'queryString' | '@webhook'
      delimiter ::= '.' | '#' | ':'
      WS ::= ' '
      NL ::= '\n'
      INDENT ::= INDENT
*/

//just playing for now: dubious implementation

class LexicalAnalyzer {

  def tokenize(input: String): List[Token] = {
    require(!input.isEmpty)

    val state = new LexicalAnalysisState(input)

    object Keyword {
      def unapply(char: Char): Option[KeywordToken] = {
        if (state.isId) {
          val lexeme = state.takeStringTillSeparator
          val token = lexeme match {
            case l@Token.Keyword.`namespace`.keyword => Some(Token.Keyword.`namespace`(state.currentPos, l.length))
            case l@Token.Keyword.`description`.keyword => Some(Token.Keyword.`description`(state.currentPos, l.length))
            case l@Token.Keyword.`get`.keyword => Some(Token.Keyword.`get`(state.currentPos, l.length))
            case l@Token.Keyword.`queryString`.keyword => Some(Token.Keyword.`queryString`(state.currentPos, l.length))
            case l@Token.Keyword.`@webhook`.keyword => Some(Token.Keyword.`@webhook`(state.currentPos, l.length))
            case _ => None
          }
          token.map(t => {state.move(t.length); t})
        }
        None
      }
    }

    object Delimiter {
      def unapply(char: Char): Option[Token.Delimiter.DelimiterToken] = {
        val token = state.currentChar match {
          case Token.Delimiter.`.`.char => Some(Token.Delimiter.`.`(state.currentPos))
          case Token.Delimiter.`#`.char => Some(Token.Delimiter.`#`(state.currentPos))
          case Token.Delimiter.`:`.char => Some(Token.Delimiter.`:`(state.currentPos))
          case _ => None
        }
        token.map(t => {state.moveNext; t})
      }
    }

    object WS {
      def unapply(char: Char): Option[Token] = {
        var isWhiteSpace = false
        while (Token.WS.chars.contains(state.currentChar)) {
          isWhiteSpace = true
          state.moveNext
        }
        if (isWhiteSpace) Some(Token.WS(state.currentPos)) else None
      }
    }

    while (state.currentPos <= input.length) {
      //todo: proper order?
      state.currentChar match {
        case Delimiter(token) => state.addToken(token)
        case WS(token) => state.addToken(token)
        case Keyword(token) => state.addToken(token)
      }
    }

    state.extractResultingTokens
  }

  class LexicalAnalysisState(input: String) {
    private[this] var _currentPos = 0
    private[this] var _tokens = ArrayBuffer.empty[Token]

    def currentPos = _currentPos
    def extractResultingTokens = _tokens.toList

    val separatorChars = Token.Delimiter.chars ++: Token.WS.chars ++: Token.NL.chars

    def currentChar = input(_currentPos)
    def hasNext = (currentPos + 1) < input.length
    def moveNext = _currentPos = _currentPos + 1
    def move(shift: Int) = _currentPos = _currentPos + shift
    def addToken(token: Token) = _tokens += token

    def takeStringTillSeparator: String = {
      var pos = _currentPos
      while (!separatorChars.contains(input(pos))) {
        pos = pos + 1
      }
      input.substring(_currentPos, pos)
    }

    /*def somethingAheadExceptWS: Boolean = {
      import scala.util.control.Breaks._
      if (hasNext) {
        var pos = _currentPos + 1
        var hasSomething = false
        breakable {
          while (!Token.NL.chars.contains(input(pos))) {
            if (!Token.WS.chars.contains(input(pos))) {
              hasSomething = true
              break
            }
            pos = pos + 1
          }
        }
        hasSomething
      }
      else false
    }*/

    def isId = {
      val isDigit = currentChar.isDigit
      val isLetter = currentChar.isLetter && currentChar <= 'z'
      isDigit || isLetter
    }
  }
}
