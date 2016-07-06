package com.ialekseev.bob

import com.ialekseev.bob.Token.{Dictionary, StringLiteral, Identifier, Variable}

package object lexical {
  def isLetter(char: Char): Boolean = char.isLetter && char <= 'z'
  def isDigit(char: Char): Boolean = char.isDigit

  def isId(char: Char): Boolean = isLetter(char) || isDigit(char) || (char == '@')
  def isIdentifier(str: String): Boolean = str.forall(isId(_))
  def identifier(str: String): Option[Token] = if (isIdentifier(str)) Some(Identifier(str)) else None

  def isVariableStart(char: Char): Boolean = char == Token.Variable.char
  def variable(str: String): Option[Token] = {
    if (str.length > 1) {
      val head = str(0)
      val tail = str.substring(1)
      if (isVariableStart(head) && isIdentifier(tail)) Some(Variable(tail)) else None
    } else None
  }

  def isStringLiteralChar(char: Char) = char == Token.StringLiteral.char
  def stringLiteral(str: String): Option[Token] = {
    if (str.length > 1) {
      val head = str(0)
      val content = str.substring(1, str.length - 1)
      val last = str(str.length - 1)
      if (isStringLiteralChar(head) && isStringLiteralChar(last)) Some(StringLiteral(content)) else None
    } else None
  }

  def isDictionaryStartChar(char: Char) = char == Token.Dictionary.startChar
  def isDictionaryEndChar(char: Char) = char == Token.Dictionary.endChar
  def dictionary(str: String): Option[Token] = {
    if (str.length > 1) {
      val head = str(0)
      val last = str(str.length - 1)
      if (isDictionaryStartChar(head) && isDictionaryEndChar(last)) Some(Dictionary(str)) else None
    } else None
  }

  def keyword(str: String): Option[Token] = {
    str match {
      case l@Token.Keyword.`namespace`.word => Some(Token.Keyword.`namespace`)
      case l@Token.Keyword.`description`.word => Some(Token.Keyword.`description`)
      case l@Token.Keyword.`@webhook`.word => Some(Token.Keyword.`@webhook`)
      case l@Token.Keyword.`method`.word => Some(Token.Keyword.`method`)
      case l@Token.Keyword.`uri`.word => Some(Token.Keyword.`uri`)
      case l@Token.Keyword.`queryString`.word => Some(Token.Keyword.`queryString`)
      case _ => None
    }
  }

  def delimiter(char: Char): Option[Token] = {
    char match {
      case Token.Delimiter.`.`.char => Some(Token.Delimiter.`.`)
      case Token.Delimiter.`#`.char => Some(Token.Delimiter.`#`)
      case Token.Delimiter.`:`.char => Some(Token.Delimiter.`:`)
      case _ => None
    }
  }

  def isNL(char: Char): Boolean = Token.NL.chars.contains(char)
  def isWS(char: Char): Boolean = Token.WS.chars.contains(char)
  def isSOT(char: Char) = char == SOT
  def isEOT(char: Char) = char == EOT

  val separatorChars = EOT +: Token.Delimiter.chars ++: Token.WS.chars ++: Token.NL.chars
  def isSeparator(char: Char): Boolean = separatorChars.contains(char)
}
