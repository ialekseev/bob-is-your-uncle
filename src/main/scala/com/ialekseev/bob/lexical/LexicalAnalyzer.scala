package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token
import scalaz._

/*[Lexical Syntax in EBNF form]
      upper ::=  'A' |...| 'Z'
      lower ::=  'a' |...| 'z'
      letter ::= upper | lower
      digit ::=  '0' |...| '9'
      id ::= letter | digit | @
      WS ::= ' '
      NL ::= '\n'

      identifier ::= id {id}
      variable ::= '$' identifier
      stringLiteral ::= "..."
      keyword ::= 'namespace' | 'description' | 'get' | 'queryString' | '@webhook'
      delimiter ::= '.' | '#' | ':'
      INDENT ::= NL{WS}
*/

trait LexicalAnalyzer {
  def tokenize(input: String): \/[Seq[LexerError], Seq[LexerToken]]
}

case class LexerToken(token: Token, offset: Int)
case class LexerError(startOffset: Int, endOffset: Int)