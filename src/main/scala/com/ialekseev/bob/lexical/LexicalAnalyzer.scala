package com.ialekseev.bob.lexical

import com.ialekseev.bob.{LexerError, LexerToken}
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
      dictionary ::= [...]
      keyword ::= 'namespace' | 'description' | '@webhook' | 'method' | 'uri' | 'queryString'
      delimiter ::= '.' | '#' | ':'
      INDENT ::= NL{WS}
*/

trait LexicalAnalyzer {
  def tokenize(input: String): \/[Seq[LexerError], Seq[LexerToken]]
}