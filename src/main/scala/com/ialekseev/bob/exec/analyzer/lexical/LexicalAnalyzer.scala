package com.ialekseev.bob.exec.analyzer.lexical

import com.ialekseev.bob.LexicalAnalysisFailed
import com.ialekseev.bob.exec.analyzer.LexerToken

import scalaz._

/*[Lexical Syntax in EBNF form]
      upper ::=  'A' |...| 'Z'
      lower ::=  'a' |...| 'z'
      letter ::= upper | lower
      digit ::=  '0' |...| '9'
      id ::= letter | digit | @
      varFirst ::= '$'
      varSecond ::= letter
      varRest ::= letter | digit
      WS ::= ' '
      NL ::= '\n'

      identifier ::= id {id}
      variable ::= varFirst varSecond {varRest}
      stringLiteral ::= "..."
      dictionary ::= [...]
      json ::= ~...~
      type ::= stringLiteral | dictionary | json
      keyword ::= 'namespace' | 'description' | '@webhook' | 'method' | 'uri' | 'queryString' | 'headers' | 'body'
      block ::= '@process...'
      delimiter ::= '.' | '#' | ':'
      INDENT ::= NL{WS}
*/

trait LexicalAnalyzer {
  def tokenize(source: String): LexicalAnalysisFailed \/ List[LexerToken]
}