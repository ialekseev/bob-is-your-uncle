package com.ialekseev.bob.lexical

import com.ialekseev.bob._
import scala.annotation.tailrec
import scalaz._
import Scalaz._

private[lexical] trait AdHocLexicalAnalysisState {
  protected type LexerState[A] =  State[LexerStateInternal, A]

  protected case class LexerStateInternal(private val raw: String, position: Int, tokens: Seq[LexerToken], errorOffsets: Seq[Int]) {
    require(raw.nonEmpty)
    require(position >= 0)
    val input = SOT + raw + EOT
  }

  protected def moveNext: LexerState[Unit] = modify(s => s.copy(position = s.position + 1))
  protected def move(shift: Int): LexerState[Unit] = modify(s => s.copy(position = s.position + shift))
  protected def jump(newPosition: Int): LexerState[Unit] = modify(s => s.copy(position = newPosition))

  protected def addToken(token: Token, offset: Int): LexerState[Unit] = modify(s => s.copy(tokens =  s.tokens :+ LexerToken(token, offset)))
  protected def addErrorOffset(errorOffset: Int): LexerState[Unit] = modify(s => s.copy(errorOffsets = s.errorOffsets :+ errorOffset))

  protected def look(position: Int, mover: Int => Int, what: Char => Boolean): LexerState[Option[(Char, Int)]] = {
    import scala.util.control.Breaks._
    get.map(s => {
      def withinBounds(pos: Int) =  pos >= 0 && pos < s.input.length

      var res: Option[(Char, Int)] = none
      var pos = mover(position)
      breakable {
        while (withinBounds(pos)) {
          if (what(s.input(pos))) {
            res = some(s.input(pos), pos)
            break
          }
          pos = mover(pos)
        }
      }
      res
    })
  }

  protected def takeAhead(till: Seq[(Char => Boolean)], last: (Int => Int)): LexerState[Option[String]]  = {
    get.flatMap(s => {
      lookAhead(char => till.exists(_(char))).map(ahead => {
        ahead.map(sep => s.input.substring(s.position, last(sep._2)))
      })
    })
  }


  protected def lookAhead(what: Char => Boolean, position: Int): LexerState[Option[(Char, Int)]] = look(position, _ + 1, what)
  protected def lookBack(what: Char => Boolean, position: Int): LexerState[Option[(Char, Int)]] = look(position, _ - 1, what)

  protected def lookAhead(what: Char => Boolean): LexerState[Option[(Char, Int)]] = get.flatMap(s => lookAhead(what, s.position))
  protected def lookBack(what: Char => Boolean): LexerState[Option[(Char, Int)]] = get.flatMap(s => lookBack(what, s.position))

  protected def takeAheadExcludingLast(till: (Char => Boolean)*): LexerState[Option[String]] = takeAhead(till, identity)
  protected def takeAheadIncludingLast(till: (Char => Boolean)*): LexerState[Option[String]] = takeAhead(till, _ + 1)

  protected def hasCurrent: LexerState[Boolean] = currentChar.map(!isEOT(_))
  protected def currentChar: LexerState[Char] = get.map(s => s.input(s.position))
  protected def currentIsFirstChar: LexerState[Boolean] = get.map(s => s.position == 0)
  protected def currentIsId: LexerState[Boolean]  = currentChar.map(isId(_))
  protected def currentIsVariableStart: LexerState[Boolean]  = currentChar.map(isVariableStart(_))
  protected def currentIsStringLiteralStart: LexerState[Boolean]  = currentChar.map(isStringLiteralChar(_))
  protected def currentIsNL: LexerState[Boolean]  = currentChar.map(isNL(_))
  protected def currentIsWS: LexerState[Boolean]  = currentChar.map(isWS(_))

  def extractResultingTokens: LexerState[Seq[LexerToken]] = get.map(_.tokens.map(c => c.copy(offset = c.offset - 1)))

  def extractErrors: LexerState[Seq[LexerError]] = {
    @tailrec def aggregateSpans(remaining: Vector[Int], openSpan: Vector[Int], res: Vector[LexerError]): Vector[LexerError] = {
      if (remaining.nonEmpty) {
        remaining match {
          case x +: xs if openSpan.lastOption.isDefined && (x - openSpan.last === 1) => aggregateSpans(xs, openSpan :+ x, res)
          case x +: xs if openSpan.nonEmpty => aggregateSpans(xs, Vector(x), res :+ LexerError(openSpan.head - 1, openSpan.last - 1))
          case x +: xs  => aggregateSpans(xs, Vector(x), res)
        }
      } else {
        if (openSpan.nonEmpty) res :+ LexerError(openSpan.head - 1, openSpan.last - 1)
        else res
      }
    }
    get.map(s => aggregateSpans(s.errorOffsets.toVector, Vector.empty, Vector.empty))
  }
}

