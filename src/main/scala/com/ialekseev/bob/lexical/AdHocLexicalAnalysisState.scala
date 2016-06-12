package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token
import scala.collection.mutable.ArrayBuffer

private[lexical] class AdHocLexicalAnalysisState(input: String) {
  private[this] var _currentPos = 0
  private[this] val _tokens = ArrayBuffer.empty[Token]
  private[this] val _errors = ArrayBuffer.empty[Int]

  def currentPos: Int = _currentPos
  def extractResultingTokens: List[Token] = _tokens.toList
  def extractErrors: List[LexicalAnalysisError] = {
    //todo: aggregate consecutive offsets
    ???
  }

  def currentChar: Char = input(_currentPos)

  def hasNext: Boolean = (currentPos + 1) < input.length
  def hasPrev: Boolean = (currentPos - 1) >= 0
  def moveNext: Unit = _currentPos = _currentPos + 1
  def move(shift: Int): Unit = _currentPos = _currentPos + shift
  def jump(pos: Int): Unit = _currentPos = pos
  def addToken(token: Token): Unit = _tokens += token
  def addErrorOffset(error: Int): Unit = _errors += error

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

  def currentIsId: Boolean = isId(currentChar)
  def currentIsVariableStart: Boolean = isVariableStart(currentChar)
  def currentIsStringLiteralStart: Boolean = isStringLiteralChar(currentChar)
  def currentIsNL: Boolean = isNL(currentChar)
  def currentIsWS: Boolean = isWS(currentChar)
}