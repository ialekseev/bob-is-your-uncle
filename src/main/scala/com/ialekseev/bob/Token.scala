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
  case class StringLiteral(content: String) extends Token { val length = content.length + 2 }

  object Dictionary {val startChar = '['; val endChar = ']'}
  case class Dictionary(raw: String, dic: Map[String, String]) extends Token { val length = raw.length }

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

  implicit val tokenShow: Show[Token] = Show.shows {
    case Token.Identifier(word) => word
    case Token.Variable(name) => Token.Variable.char + name
    case Token.StringLiteral(text) => Token.StringLiteral.char + text + Token.StringLiteral.char
    case keyword: Token.Keyword.KeywordToken => keyword.word
    case delimiter: Token.Delimiter.DelimiterToken => delimiter.char.toString
    case rest => rest.toString
  }
}

trait TokenTag[T] { def asString: String }

object TokenTag {
  import Token._
  implicit val identifierTag = new TokenTag[Identifier] { def asString = "identifier" }
  implicit val variableTag = new TokenTag[Variable] { def asString = "variable" }
  implicit val stringLiteralTag = new TokenTag[StringLiteral] { def asString = "string literal" }
  implicit val dictionaryTag = new TokenTag[Dictionary] { def asString = "dictionary" }
  implicit val namespaceKeywordTag = new TokenTag[Keyword.`namespace`.type] { def asString = Keyword.`namespace`.word}
  implicit val descriptionKeywordTag = new TokenTag[Keyword.`description`.type] { def asString = Keyword.`description`.word}
  implicit val webhookKeywordTag = new TokenTag[Keyword.`@webhook`.type] { def asString = Keyword.`@webhook`.word}
  implicit val uriKeywordTag = new TokenTag[Keyword.`uri`.type] { def asString = Keyword.`uri`.word}
  implicit val methodKeywordTag = new TokenTag[Keyword.`method`.type] { def asString = Keyword.`method`.word}
  implicit val queryStringKeywordTag = new TokenTag[Keyword.`queryString`.type] { def asString = Keyword.`queryString`.word}
  implicit val dotDelimiterTag = new TokenTag[Delimiter.`.`.type] { def asString = Delimiter.`.`.char.toString}
  implicit val poundDelimiterTag = new TokenTag[Delimiter.`#`.type] { def asString = Delimiter.`#`.char.toString}
  implicit val colonDelimiterTag = new TokenTag[Delimiter.`:`.type] { def asString = Delimiter.`:`.char.toString}
  implicit val indentTag = new TokenTag[INDENT] { def asString = "indent"}
}