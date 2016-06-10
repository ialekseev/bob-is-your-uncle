package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token
import scala.collection.mutable.ArrayBuffer

private[lexical] class LexicalAnalysisState(input: String) {
  private[this] var _currentPos = 0
  private[this] var _tokens = ArrayBuffer.empty[Token]
  private[this] var _errors = ArrayBuffer.empty[LexicalAnalysisError]

  def currentPos = _currentPos
  def extractResultingTokens = _tokens.toList
  def extractErrors = _errors.toList

  def currentChar = input(_currentPos)

  def hasNext = (currentPos + 1) < input.length
  def hasPrev = (currentPos - 1) >= 0
  def moveNext = _currentPos = _currentPos + 1
  def move(shift: Int) = _currentPos = _currentPos + shift
  def jump(pos: Int) = _currentPos = pos
  def addToken(token: Token) = _tokens += token
  def addError(error: LexicalAnalysisError) = _errors += error

  private def isWithinInput(pos: Int) = pos >= 0 && pos < input.length

  def look(hasAnother: => Boolean, mover: Int => Int, what: Char => Boolean): Option[(Char, Int)] = {
    if (hasAnother) {
      var pos = mover(_currentPos)
      while (isWithinInput(pos)) {
        if (what(input(pos))) {
          return Some(input(pos), pos)
        }
        pos = mover(pos)
      }
    }
    None
  }

  def lookAhead(what: Char => Boolean): Option[(Char, Int)]  = look(hasNext, _ + 1, what)
  def lookBack(what: Char => Boolean): Option[(Char, Int)]  = look(hasPrev, _ - 1, what)

  def takeAhead(till: Seq[(Char => Boolean)], last: (Int => Int)): Option[String] = {
    lookAhead(char => till.exists(_(char))).map(sep => input.substring(_currentPos, sep._2))
  }

  def takeAheadExcludingLast(till: (Char => Boolean)*): Option[String] = takeAhead(till, identity)
  def takeAheadIncludingLast(till: (Char => Boolean)*): Option[String] = takeAhead(till, _ + 1)

  def currentIsId = isId(currentChar)
  def currentIsVariableStart = isVariableStart(currentChar)
  def currentIsStringLiteralStart = isStringLiteralChar(currentChar)
  def currentIsNL = isNL(currentChar)
}
