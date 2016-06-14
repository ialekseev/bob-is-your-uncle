package com.ialekseev.bob

import com.ialekseev.bob.Token.Delimiter.DelimiterToken
import com.ialekseev.bob.Token.Keyword.KeywordToken
import com.ialekseev.bob.Token.{StringLiteral, Identifier, Variable}

package object lexical {
  def isLetter(char: Char): Boolean = char.isLetter && char <= 'z'
  def isDigit(char: Char): Boolean = char.isDigit

  def isId(char: Char): Boolean = isDigit(char) || isLetter(char)
  def isIdentifier(str: String): Boolean = str.forall(isId(_))
  def identifier(str: String): Option[(Int => Identifier)] = if (isIdentifier(str)) Some(Identifier(str, _, str.length)) else None

  def isVariableStart(char: Char): Boolean = char == Token.Variable.char
  def variable(str: String): Option[(Int => Variable)] = {
    if (str.length > 1) {
      val head = str(0)
      val tail = str.substring(1)
      if (isVariableStart(head) && isIdentifier(tail)) Some(Variable(tail, _, str.length)) else None
    } else None
  }

  def isStringLiteralChar(char: Char) = char == Token.StringLiteral.char
  def stringLiteral(str: String): Option[(Int => StringLiteral)] = {
    if (str.length > 1) {
      val head = str(0)
      val content = str.substring(1, str.length - 1)
      val last = str(str.length - 1)
      if (isStringLiteralChar(head) && isStringLiteralChar(last)) Some(StringLiteral(content, _, str.length)) else None
    } else None
  }

  def keyword(str: String): Option[(Int => KeywordToken)] = {
    str match {
      case l@Token.Keyword.`namespace`.keyword => Some(Token.Keyword.`namespace`(_))
      case l@Token.Keyword.`description`.keyword => Some(Token.Keyword.`description`(_))
      case l@Token.Keyword.`get`.keyword => Some(Token.Keyword.`get`(_))
      case l@Token.Keyword.`queryString`.keyword => Some(Token.Keyword.`queryString`(_))
      case l@Token.Keyword.`@webhook`.keyword => Some(Token.Keyword.`@webhook`(_))
      case _ => None
    }
  }

  def delimiter(char: Char): Option[(Int => DelimiterToken)] = {
    char match {
      case Token.Delimiter.`.`.char => Some(Token.Delimiter.`.`(_))
      case Token.Delimiter.`#`.char => Some(Token.Delimiter.`#`(_))
      case Token.Delimiter.`:`.char => Some(Token.Delimiter.`:`(_))
      case _ => None
    }
  }

  def isNL(char: Char): Boolean = Token.NL.chars.contains(char)
  def isWS(char: Char): Boolean = Token.WS.chars.contains(char)
  def isEOT(char: Char) = char == EOT

  val separatorChars = EOT +: Token.Delimiter.chars ++: Token.WS.chars ++: Token.NL.chars
  def isSeparator(char: Char): Boolean = separatorChars.contains(char)
}
