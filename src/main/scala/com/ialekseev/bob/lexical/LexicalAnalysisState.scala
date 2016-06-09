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

  def takeStringTillSeparator: String = {
    var pos = _currentPos
    while (isNonSeparator(input(pos))) {
      pos = pos + 1
    }
    input.substring(_currentPos, pos)
  }

  def find(hasAnother: => Boolean, mover: Int => Int, what: Char => Boolean): Option[(Char, Int)] = {
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

  def findNext(what: Char => Boolean) = find(hasNext, _ + 1, what)
  def findPrev(what: Char => Boolean) = find(hasPrev, _ - 1, what)

  def currentIsId = isId(currentChar)
  def currentIsVariableStart = isVariableStart(currentChar)
}
