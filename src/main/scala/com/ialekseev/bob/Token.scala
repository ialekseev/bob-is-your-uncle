package com.ialekseev.bob

import scalaz.Show

trait Token {
  val length: Int
}

object Token {
  case class Identifier(word: String) extends Token { val length = word.length }

  object Variable {val char = '$'}
  case class Variable(name: String) extends Token { val length = name.length + 1 }

  object StringLiteral { val char = '"' }
  case class StringLiteral(text: String) extends Token { val length = text.length + 2 }

  object Keyword {
    trait KeywordToken extends Token {
      val word: String
      lazy val length = word.length
    }

    case object `namespace` extends KeywordToken{ val word = "namespace"}
    case object `description` extends KeywordToken { val word = "description" }
    case object `@webhook` extends KeywordToken { val word = "@webhook" }
    case object `uri` extends KeywordToken { val word = "uri"}
    case object `method` extends KeywordToken { val word = "method"}
    case object `queryString` extends KeywordToken { val word = "queryString" }
  }

  object Delimiter {
    trait DelimiterToken extends Token {
      val char: Char
      val length = 1
    }

    case object `.` extends DelimiterToken { val char = '.' }
    case object `#` extends DelimiterToken { val char = '#' }
    case object `:` extends DelimiterToken { val char = ':' }
    val chars = Seq(`.`.char, `#`.char, `:`.char)
  }

  object WS { val chars = Seq(' ', '\t')}
  object NL { val chars = Seq('\n','\r', SOT, EOT)}
  case class INDENT(length: Int) extends Token

  implicit def tokenShow: Show[Token] = Show.shows {
    case Token.Identifier(word) => word
    case Token.Variable(name) => Token.Variable.char + name
    case Token.StringLiteral(text) => Token.StringLiteral.char + text + Token.StringLiteral.char
    case k: Token.Keyword.KeywordToken => k.word
    case d: Token.Delimiter.DelimiterToken => d.char.toString
    case rest => rest.toString
  }
}