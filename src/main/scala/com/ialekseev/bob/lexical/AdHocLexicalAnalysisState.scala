package com.ialekseev.bob.lexical

import com.ialekseev.bob._
import scala.annotation.tailrec
import scalaz._
import Scalaz._

private[lexical] trait AdHocLexicalAnalysis {
  type LexerState[S] =  State[LexerStateInternal, S]


  protected case class LexerStateInternal(private val raw: String, position: Int, tokens: List[Token], errorOffsets: List[Int]) {
    require(raw.nonEmpty)
    require(position >= 0)
    val input = SOT + raw + EOT
  }

  protected def moveNext: LexerState[Unit] = modify(s => s.copy(position = s.position + 1))
  protected def move(shift: Int): LexerState[Unit] = modify(s => s.copy(position = s.position + shift))
  protected def jump(newPosition: Int): LexerState[Unit] = modify(s => s.copy(position = newPosition))

  protected def addToken(token: Token): LexerState[Unit] = modify(s => s.copy(tokens = token :: s.tokens))
  protected def addErrorOffset(errorOffset: Int): LexerState[Unit] = modify(s => s.copy(errorOffsets = errorOffset :: s.errorOffsets))

  protected def look(position: Int, mover: Int => Int, what: Char => Boolean): LexerState[Option[(Char, Int)]] = {
    import scala.util.control.Breaks._
    get.map(s => {
      def withinBounds(pos: Int) =  pos >=0 && pos < s.input.length

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


  protected def lookAhead(what: Char => Boolean): LexerState[Option[(Char, Int)]] = get.flatMap(s => look(s.position, _ + 1, what))
  protected def lookBack(what: Char => Boolean): LexerState[Option[(Char, Int)]] = get.flatMap(s => look(s.position, _ - 1, what))
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

  def extractResultingTokens: LexerState[List[Token]] = get.map(_.tokens.reverse)
  def extractErrors: LexerState[List[LexicalAnalysisError]] = {
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

    get.map(s => {
      s.errorOffsets match {
        case Nil => Nil
        case list => aggregateSpans(list.zip(list.tail), Nil)
      }
    })
  }
}

