package com.ialekseev.bob

import com.ialekseev.bob.Token._
import scala.util.Try

package object lexical {
  def isLetter(char: Char): Boolean = char.isLetter && char <= 'z'
  def isDigit(char: Char): Boolean = char.isDigit

  def isId(char: Char): Boolean = isLetter(char) || isDigit(char) || (char == '@')
  def isIdentifier(str: String): Boolean = str.forall(isId(_))
  def identifier(str: String): Option[Token] = if (isIdentifier(str)) Some(Identifier(str)) else None

  def isVariableStart(char: Char): Boolean = char == Token.Variable.char
  def variable(str: String): Option[Token] = {
    if (str.length > 1) {
      val (head, tail) = str.dismantle2
      if (isVariableStart(head) && isIdentifier(tail)) Some(Variable(tail)) else None
    } else None
  }

  def isStringLiteralChar(char: Char) = char == Type.StringLiteral.char
  def stringLiteral(str: String): Option[Token] = {
    if (str.length > 1) {
      val (head, content, last) = str.dismantle3
      if (isStringLiteralChar(head) && isStringLiteralChar(last)) Some(Type.StringLiteral(content)) else None
    } else None
  }

  def isDictionaryStartChar(char: Char) = char == Type.Dictionary.startChar
  def isDictionaryEndChar(char: Char) = char == Type.Dictionary.endChar
  def dictionary(str: String): Option[Token] = {
    if (str.length > 1) {
      val (head, content, last) = str.dismantle3
      if (isDictionaryStartChar(head) && isDictionaryEndChar(last)) {
        import org.json4s._
        import org.json4s.native.JsonMethods._
        implicit val formats = org.json4s.DefaultFormats
        val jsonStr = "{" + content + "}"
        Try(parse(jsonStr).extract[Map[String, String]]).toOption.map(Type.Dictionary(str, _))
      } else None
    } else None
  }

  def isJsonStartChar(char: Char) = char == Type.Json.char
  def isJsonEndChar(char: Char) = char == Type.Json.char
  def json(str: String): Option[Token] = {
    if (str.length > 1) {
      val (head, content, last) = str.dismantle3
      if (isJsonStartChar(head) && isJsonEndChar(last)) {
        import org.json4s._
        import org.json4s.native.JsonMethods._
        implicit val formats = org.json4s.DefaultFormats
        Try(parse(content)).toOption.map(Type.Json(str, _))
      } else None
    } else None
  }

  def keyword(str: String): Option[Token] = {
    str match {
      case l@Token.Keyword.`namespace`.word => Some(Token.Keyword.`namespace`)
      case l@Token.Keyword.`description`.word => Some(Token.Keyword.`description`)
      case l@Token.Keyword.`@webhook`.word => Some(Token.Keyword.`@webhook`)
      case l@Token.Keyword.`uri`.word => Some(Token.Keyword.`uri`)
      case l@Token.Keyword.`method`.word => Some(Token.Keyword.`method`)
      case l@Token.Keyword.`headers`.word => Some(Token.Keyword.`headers`)
      case l@Token.Keyword.`queryString`.word => Some(Token.Keyword.`queryString`)
      case l@Token.Keyword.`body`.word => Some(Token.Keyword.`body`)
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
