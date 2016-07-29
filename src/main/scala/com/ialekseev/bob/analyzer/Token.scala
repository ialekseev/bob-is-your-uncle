package com.ialekseev.bob.analyzer

import org.json4s.JsonAST.JValue
import scalaz.Show

trait Token {
  val length: Int
}

object Token {
  case class Identifier(word: String) extends Token { val length = word.length }

  object Variable { val char = '$' }
  case class Variable(name: String) extends Token { val length = name.length + 1 }

  object Type {
    object StringLiteral { val char = '"' }
    case class StringLiteral(content: String) extends Token { val length = content.length + 2 }

    object Dictionary { val startChar = '['; val endChar = ']' }
    case class Dictionary(raw: String, dic: Map[String, String]) extends Token { val length = raw.length }

    object Json { val char = '~' }
    case class Json(raw: String, json: JValue) extends Token { val length = raw.length}
  }

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
    case object `headers` extends KeywordToken { val word = "headers" }
    case object `queryString` extends KeywordToken { val word = "queryString" }
    case object `body` extends KeywordToken { val word = "body" }

    case object `@process` extends KeywordToken { val word = "@process" }
  }

  object Block {
    val wordStartChar = '<'
    val wordEndChar = '>'
    val endWord = wordStartChar + "end" + wordEndChar

    trait BlockToken extends Token {
      val beginWord: String
      val content: String
      lazy val length = beginWord.length + content.length + endWord.length
    }

    object `<scala>` { val beginWord =  wordStartChar + "scala" + wordEndChar }
    case class `<scala>`(content: String) extends BlockToken { val beginWord = `<scala>`.beginWord }
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
    case Token.Type.StringLiteral(text) => Token.Type.StringLiteral.char + text + Token.Type.StringLiteral.char
    case keyword: Token.Keyword.KeywordToken => keyword.word
    case block: Token.Block.BlockToken => block.beginWord + "..." + Token.Block.endWord
    case delimiter: Token.Delimiter.DelimiterToken => delimiter.char.toString
    case rest => rest.toString
  }
}

case class LexerToken(token: Token, offset: Int)

trait TokenTag[T] { def asString: String }
object TokenTag {
  import Token._
  implicit val identifierTag = new TokenTag[Identifier] { def asString = "identifier" }
  implicit val variableTag = new TokenTag[Variable] { def asString = "variable" }

  implicit val stringLiteralTag = new TokenTag[Type.StringLiteral] { def asString = "string literal" }
  implicit val dictionaryTag = new TokenTag[Type.Dictionary] { def asString = "dictionary" }
  implicit val jsonTag = new TokenTag[Type.Json] { def asString = "json" }

  implicit val namespaceKeywordTag = new TokenTag[Keyword.`namespace`.type] { def asString = Keyword.`namespace`.word}
  implicit val descriptionKeywordTag = new TokenTag[Keyword.`description`.type] { def asString = Keyword.`description`.word}

  implicit val webhookKeywordTag = new TokenTag[Keyword.`@webhook`.type] { def asString = Keyword.`@webhook`.word}
  implicit val uriKeywordTag = new TokenTag[Keyword.`uri`.type] { def asString = Keyword.`uri`.word}
  implicit val methodKeywordTag = new TokenTag[Keyword.`method`.type] { def asString = Keyword.`method`.word}
  implicit val headersKeywordTag = new TokenTag[Keyword.`headers`.type] { def asString = Keyword.`headers`.word}
  implicit val queryStringKeywordTag = new TokenTag[Keyword.`queryString`.type] { def asString = Keyword.`queryString`.word}
  implicit val bodyKeywordTag = new TokenTag[Keyword.`body`.type] { def asString = Keyword.`body`.word}

  implicit val processKeywordTag = new TokenTag[Keyword.`@process`.type] { def asString = Keyword.`@process`.word}
  implicit val scalaBlockTag = new TokenTag[Block.`<scala>`] { def asString = Block.`<scala>`.beginWord + "..." + Block.endWord}

  implicit val dotDelimiterTag = new TokenTag[Delimiter.`.`.type] { def asString = Delimiter.`.`.char.toString}
  implicit val poundDelimiterTag = new TokenTag[Delimiter.`#`.type] { def asString = Delimiter.`#`.char.toString}
  implicit val colonDelimiterTag = new TokenTag[Delimiter.`:`.type] { def asString = Delimiter.`:`.char.toString}

  implicit val indentTag = new TokenTag[INDENT] { def asString = "indent"}
}