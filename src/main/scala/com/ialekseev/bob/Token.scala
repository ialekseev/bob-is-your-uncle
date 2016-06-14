package com.ialekseev.bob

trait Token {
  val offset: Int
  val length: Int
}

trait CharToken extends Token {
  val length = 1
}

object Token {
  case class Identifier(name: String, offset: Int, length: Int) extends Token

  object Variable {val char = '$'}
  case class Variable(name: String, offset: Int, length: Int) extends Token

  object StringLiteral { val char = '"' }
  case class StringLiteral(text: String, offset: Int, length: Int) extends Token

  object Keyword {
    abstract class KeywordToken(val length: Int) extends Token

    object `namespace` extends { val keyword = "namespace" }
    case class `namespace`(offset: Int) extends KeywordToken(`namespace`.keyword.length)

    object `description` { val keyword = "description" }
    case class `description`(offset: Int) extends KeywordToken(`description`.keyword.length)

    object `get` { val keyword = "get"}
    case class `get`(offset: Int) extends KeywordToken(`get`.keyword.length)

    object `queryString` { val keyword = "queryString" }
    case class `queryString`(offset: Int) extends KeywordToken(`queryString`.keyword.length)

    object `@webhook` { val keyword = "@webhook" }
    case class `@webhook`(offset: Int) extends KeywordToken(`@webhook`.keyword.length)
  }

  object Delimiter {
    trait DelimiterToken extends CharToken

    object `.` { val char = '.' }
    case class `.`(offset: Int) extends DelimiterToken

    object `#` { val char = '#' }
    case class `#`(offset: Int) extends DelimiterToken

    object `:` { val char = ':' }
    case class `:`(offset: Int) extends DelimiterToken

    val chars = Seq(`.`.char, `#`.char, `:`.char)
  }

  object WS { val chars = Seq(' ')}
  object NL { val chars = Seq('\n', EOT)}
  case class INDENT(offset: Int, level: Int) extends CharToken
}
