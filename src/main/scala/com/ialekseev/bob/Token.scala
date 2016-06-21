package com.ialekseev.bob

trait Token {
  val length: Int
}

trait CharToken extends Token {
  val char: Char
  val length = 1
}

trait WordToken extends  Token {
  val word: String
  lazy val length = word.length
}

object Token {
  case class Identifier(word: String) extends WordToken

  object Variable {val char = '$'}
  case class Variable(name: String) extends Token { val length = name.length + 1 }

  object StringLiteral { val char = '"' }
  case class StringLiteral(text: String) extends Token { val length = text.length + 2 }

  object Keyword {
    case object `namespace` extends WordToken{ val word = "namespace"}
    case object `description` extends WordToken { val word = "description" }
    case object `get` extends WordToken { val word = "get"}
    case object `queryString` extends WordToken { val word = "queryString" }
    case object `@webhook` extends WordToken { val word = "@webhook" }
  }

  object Delimiter {
    case object `.` extends CharToken { val char = '.' }
    case object `#` extends CharToken { val char = '#' }
    case object `:` extends CharToken { val char = ':' }
    val chars = Seq(`.`.char, `#`.char, `:`.char)
  }

  object WS { val chars = Seq(' ', '\t')}
  object NL { val chars = Seq('\n','\r', SOT, EOT)}
  case class INDENT(length: Int) extends Token
}