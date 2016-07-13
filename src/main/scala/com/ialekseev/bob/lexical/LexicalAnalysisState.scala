package com.ialekseev.bob.lexical

import com.ialekseev.bob._
import com.ialekseev.bob.lexical.LexicalAnalyzer._
import scalaz._
import Scalaz._

private[lexical] trait LexicalAnalysisState {
  protected type LexerState[A] =  State[LexerStateInternal, A]

  protected case class LexerStateInternal(private val raw: String, position: Int, tokens: Seq[LexerToken], errors: Seq[LexerError]) {
    require(raw.nonEmpty)
    require(position >= 0)
    val input = SOT + raw + EOT
  }

  protected case class Tokenized(token: Token, offset: Int, movePosition: Int) {
    require(movePosition > 0)
  }

  protected def moveNext: LexerState[Unit] = modify(s => s.copy(position = s.position + 1))
  protected def move(shift: Int): LexerState[Unit] = modify(s => s.copy(position = s.position + shift))
  protected def jump(newPosition: Int): LexerState[Unit] = modify(s => s.copy(position = newPosition))

  protected def addToken(token: Token, offset: Int): LexerState[Unit] = modify(s => s.copy(tokens =  s.tokens :+ LexerToken(token, offset)))
  protected def addError(startOffset: Int, endOffset: Int): LexerState[Unit] = modify(s => s.copy(errors = s.errors :+ LexerError(startOffset, endOffset)))

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

  protected def lookAhead(what: Char => Boolean): LexerState[Option[(Char, Int)]] = get[LexerStateInternal] >>= (s => lookAhead(what, s.position))
  protected def lookBack(what: Char => Boolean): LexerState[Option[(Char, Int)]] = get[LexerStateInternal] >>= (s => lookBack(what, s.position))

  protected def takeAheadExcludingLast(till: (Char => Boolean)*): LexerState[Option[String]] = takeAhead(till, identity)
  protected def takeAheadIncludingLast(till: (Char => Boolean)*): LexerState[Option[String]] = takeAhead(till, _ + 1)

  protected def hasCurrent: LexerState[Boolean] = currentChar.map(!isEOT(_))
  protected def currentPos: LexerState[Int] = get.map(_.position)
  protected def currentChar: LexerState[Char] = get.map(s => s.input(s.position))
  protected def currentIsFirstChar: LexerState[Boolean] = get.map(s => s.position == 0)
  protected def currentIsId: LexerState[Boolean] = currentChar.map(isId(_))
  protected def currentIsVariableStart: LexerState[Boolean] = currentChar.map(isVariableStart(_))
  protected def currentIsStringLiteralStart: LexerState[Boolean] = currentChar.map(isStringLiteralChar(_))
  protected def currentIsDictionaryStart: LexerState[Boolean] = currentChar.map(isDictionaryStartChar(_))
  protected def currentIsNL: LexerState[Boolean]  = currentChar.map(isNL(_))
  protected def currentIsWS: LexerState[Boolean]  = currentChar.map(isWS(_))

  protected def wordStep(currentIs: LexerState[Boolean], takeAhead: => LexerState[Option[String]], tokenExtractor: (String => Option[Token])): LexerState[Option[Tokenized]] = {
    currentIs >>= (is => {
      if (is) {
        for {
          ahead <- takeAhead
          token <- token(ahead >>= tokenExtractor)
        } yield token
      }
      else none[Tokenized].point[LexerState]
    })
  }

  protected def token(token: Option[Token]): LexerState[Option[Tokenized]] = {
    get.map(s => token.map(t => Tokenized(t, s.position, t.length)))
  }

  protected def addTokenAndMove(step: LexerState[Option[Tokenized]]): LexerState[Option[Unit]] = {
    val addAndMove: LexerState[Option[Tokenized]] = step >>= {
      case t@Some(tokenized) => (addToken(tokenized.token, tokenized.offset) >> move(tokenized.movePosition)) >| t
      case None => step
    }
    addAndMove.map(v => (if (v.isDefined) None else someUnit))
  }

  def extractResultingTokens: LexerState[Seq[LexerToken]] = get.map(_.tokens.map(c => c.copy(offset = c.offset - 1)))
  def extractErrors: LexerState[Seq[LexerError]] = get.map(_.errors.map(c => c.copy(startOffset = c.startOffset - 1, endOffset = c.endOffset - 1)))
}

