package com.ialekseev.bob.analyzer.lexical

import com.ialekseev.bob.analyzer.{LexerError, LexerToken}
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
      json ::= ~...~
      type ::= stringLiteral | dictionary | json
      keyword ::= 'namespace' | 'description' | '@webhook' | 'method' | 'uri' | 'queryString' | 'headers' | 'body' | @process
      block ::= '<scala>...<end>'
      delimiter ::= '.' | '#' | ':'
      INDENT ::= NL{WS}
*/

trait LexicalAnalyzer {
  def tokenize(input: String): Seq[LexerError] \/ Seq[LexerToken]
}