package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

private[lexical] class AdHocLexicalAnalysisState(input: String) {
  private[this] var _currentPos = 0
  private[this] val _tokens = ArrayBuffer.empty[Token]
  private[this] val _errorOffsets = ArrayBuffer.empty[Int]

  def currentPos: Int = _currentPos
  def currentChar: Char = input(_currentPos)

  def extractResultingTokens: List[Token] = _tokens.toList
  def extractErrors: List[LexicalAnalysisError] = {
    @tailrec def aggregateSpans(remaining: List[(Int, Int)], res: List[LexicalAnalysisError]): List[LexicalAnalysisError] = {
      val spanned = remaining.span(el => el._2 - el._1 == 1)
      val (lexicalError, rest) = spanned._1 match {
        case Nil => (LexicalAnalysisError(remaining.head._1, remaining.head._1), spanned._2.tail)
        case span => (LexicalAnalysisError(span.head._1, span.last._2), spanned._2)
      }
      rest match {
        case Nil => List(lexicalError)
        case _ => aggregateSpans(rest, lexicalError :: res)
      }
    }

    _errorOffsets.toList match {
      case Nil => Nil
      case list => aggregateSpans(list.zip(list.tail), Nil)
    }
  }

  def hasCurrent: Boolean = currentPos < input.length
  def hasNext: Boolean = (currentPos + 1) < input.length
  def hasPrev: Boolean = (currentPos - 1) >= 0
  def moveNext: Unit = _currentPos = _currentPos + 1
  def move(shift: Int): Unit = _currentPos = _currentPos + shift
  def jump(pos: Int): Unit = _currentPos = pos
  def addToken(token: Token): Unit = _tokens += token
  def addErrorOffset(errorOffset: Int): Unit = {
    require(errorOffset >= 0)
    _errorOffsets += errorOffset
  }

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