package com.ialekseev.bob.lexical

import com.ialekseev.bob._
import com.ialekseev.bob.Token
import scala.annotation.tailrec
import scalaz._
import Scalaz._

private[lexical] object AdHocLexicalAnalysisState {
  type LexerState[S] =  State[LexerStateInternal, S]

  case class LexerStateInternal private (input: String, position: Int, tokens: List[Token], errorOffsets: List[Int])

  def apply(): LexerState[Unit] = {
    modify(s => {
      if (!isEOT(s.input.last))
        s.copy(input = s.input + EOT)
      else s
    })
  }

  trait AdHocLexicalAnalysis {
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

    def currentChar: LexerState[Char] = get.map(s => s.input(s.position))

    def moveNext: LexerState[Unit] = modify(s => s.copy(position = s.position + 1))
    def move(shift: Int): LexerState[Unit] = modify(s => s.copy(position = s.position + shift))
    def jump(newPosition: Int): LexerState[Unit] = modify(s => s.copy(position = newPosition))

    def addToken(token: Token): LexerState[Unit] = modify(s => s.copy(tokens = token :: s.tokens))


    def addErrorOffset(errorOffset: Int): LexerState[Unit] = {
      require(errorOffset >= 0)
      modify(s => s.copy(errorOffsets = errorOffset :: s.errorOffsets))
    }

    def look(position: Int, hasAnother: (Int, String) => Boolean, mover: Int => Int, what: Char => Boolean): LexerState[Option[(Char, Int)]] = {
      import scala.util.control.Breaks._
      get.map(s => {
        def isWithinInput(pos: Int) = pos >= 0 && pos < s.input.length

        var res: Option[(Char, Int)] = none
        if (hasAnother(s.position, s.input)) {
          var pos = mover(position)
          breakable {
            while (isWithinInput(pos)) {
              if (what(s.input(pos))) {
                res = some(s.input(pos), pos)
                break
              }
              pos = mover(pos)
            }
          }
        }
        res
      })
    }

    def hasNext(position: Int, input: String): Boolean = (position < input.length - 1) && !isEOT(input(position + 1))
    def hasPrev(position: Int, input: String): Boolean = position -1 >= 0

    def hasCurrent: LexerState[Boolean] = currentIsEOT.map(!_)
    def hasNext: LexerState[Boolean] = get.map(s => (s.position < s.input.length - 1) && !isEOT(s.input(s.position + 1)))
    def hasPrev: LexerState[Boolean] = get.map(s => (s.position - 1) >= 0)

    def lookAhead(what: Char => Boolean): LexerState[Option[(Char, Int)]] = {
      get.flatMap(s => look(s.position, hasNext, _ + 1, what))
    }

    def lookBack(what: Char => Boolean): LexerState[Option[(Char, Int)]] = {
      get.flatMap(s => look(s.position, hasPrev, _ - 1, what))
    }

    def takeAhead(till: Seq[(Char => Boolean)], last: (Int => Int)): LexerState[Option[String]]  = {
      get.flatMap(s => {
        lookAhead(char => till.exists(_(char))).map(ahead => {
          ahead.map(sep => s.input.substring(s.position, sep._2))
        })
      })
    }

    def takeAheadExcludingLast(till: (Char => Boolean)*): LexerState[Option[String]] = takeAhead(till, identity)
    def takeAheadIncludingLast(till: (Char => Boolean)*): LexerState[Option[String]] = takeAhead(till, _ + 1)

    def currentIsFirstChar: LexerState[Boolean] = get.map(s => s.position == 0)
    def currentIsId: LexerState[Boolean]  = currentChar.map(isId(_))
    def currentIsVariableStart: LexerState[Boolean]  = currentChar.map(isVariableStart(_))
    def currentIsStringLiteralStart: LexerState[Boolean]  = currentChar.map(isStringLiteralChar(_))
    def currentIsNL: LexerState[Boolean]  = currentChar.map(isNL(_))
    def currentIsWS: LexerState[Boolean]  = currentChar.map(isWS(_))
    def currentIsEOT: LexerState[Boolean]  = currentChar.map(isEOT(_))
  }
}

