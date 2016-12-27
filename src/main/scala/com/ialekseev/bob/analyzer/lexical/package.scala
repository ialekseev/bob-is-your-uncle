package com.ialekseev.bob.analyzer

import com.ialekseev.bob.analyzer.Token._

import scala.util.Try
import scalaz.Scalaz._
import scalaz._

package object lexical {
  def isLetter(char: Char): Boolean = char.isLetter && char <= 'z'
  def isDigit(char: Char): Boolean = char.isDigit

  def isId(char: Char): Boolean = isLetter(char) || isDigit(char) || (char == '@')
  def isIdentifier(str: String): Boolean = str.forall(isId(_))
  def identifier(str: String): Option[Token] = if (isIdentifier(str)) Some(Identifier(str)) else None

  def isVarFirst(char: Char): Boolean = char == Token.Variable.char
  def isVarSecond(char: Char): Boolean = isLetter(char)
  def isVariableRest(str: String): Boolean = str.forall(char => isLetter(char) || isDigit(char))
  def variable(str: String): Option[Token] = {
    if (str.length > 1) {
      val (first, second, rest) = (str(0), str(1), str.substring(2))
      if (isVarFirst(first) && isVarSecond(second) && isVariableRest(rest)) Some(Variable(second + rest)) else None
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
      case l@Token.Keyword.`namespace`.word => some(Token.Keyword.`namespace`)
      case l@Token.Keyword.`description`.word => some(Token.Keyword.`description`)

      case l@Token.Keyword.`@webhook`.word => some(Token.Keyword.`@webhook`)
      case l@Token.Keyword.`uri`.word => some(Token.Keyword.`uri`)
      case l@Token.Keyword.`method`.word => some(Token.Keyword.`method`)
      case l@Token.Keyword.`headers`.word => some(Token.Keyword.`headers`)
      case l@Token.Keyword.`queryString`.word => some(Token.Keyword.`queryString`)
      case l@Token.Keyword.`body`.word => some(Token.Keyword.`body`)

      case l@Token.Keyword.`@process`.word => some(Token.Keyword.`@process`)

      case _ => None
    }
  }

  def isBlockWordStartChar(char: Char) = char == Block.wordStartChar
  def isBlockWordEndChar(char: Char) = char == Block.wordEndChar
  def block(beginWord: String, content: String): Option[Token] = {
    beginWord match {
      case Token.Block.`<scala>`.beginWord => some(Token.Block.`<scala>`(content))
      case _ => none
    }
  }

  def delimiter(char: Char): Option[Token] = {
    char match {
      case Token.Delimiter.`.`.char => some(Token.Delimiter.`.`)
      case Token.Delimiter.`#`.char => some(Token.Delimiter.`#`)
      case Token.Delimiter.`:`.char => some(Token.Delimiter.`:`)
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
