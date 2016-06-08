package com.ialekseev.bob

trait Token {
  val offset: Int
}

trait KeywordToken extends Token {
  val length: Int
}

object Token {
  case class Identifier(name: String, offset: Int) extends Token
  case class Variable(name: String, offset: Int) extends Token
  case class StringLiteral(text: String, offset: Int) extends Token

  object Keyword {
    object `namespace` extends { val keyword = "namespace" }
    case class `namespace`(offset: Int, length: Int) extends KeywordToken

    object `description` { val keyword = "description" }
    case class `description`(offset: Int, length: Int) extends KeywordToken

    object `get` { val keyword = "get"}
    case class `get`(offset: Int, length: Int) extends KeywordToken

    object `queryString` { val keyword = "queryString" }
    case class `queryString`(offset: Int, length: Int) extends KeywordToken

    object `@webhook` { val keyword = "@webhook" }
    case class `@webhook`(offset: Int, length: Int) extends KeywordToken
  }

  object Delimiter {
    trait DelimiterToken extends Token

    object `.` { val char = '.' }
    case class `.`(offset: Int) extends DelimiterToken

    object `#` { val char = '#' }
    case class `#`(offset: Int) extends DelimiterToken

    object `:` { val char = ':' }
    case class `:`(offset: Int) extends DelimiterToken

    val chars = Seq(`.`.char, `#`.char, `:`.char)
  }

  object WS { val chars = Seq(' ')}
  case class WS(offset: Int) extends Token

  object NL { val chars = Seq('\n')}
  case class NL(offset: Int) extends Token

  case class INDENT(offset: Int) extends Token
}
