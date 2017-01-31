package com.ialekseev.bob.analyzer.lexical

import com.ialekseev.bob.Models.LexicalAnalysisFailed
import com.ialekseev.bob.analyzer.LexerToken

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
      keyword ::= 'namespace' | 'description' | '@webhook' | 'method' | 'uri' | 'queryString' | 'headers' | 'body' | @process
      block ::= '<scala>...<end>'
      delimiter ::= '.' | '#' | ':'
      INDENT ::= NL{WS}
*/

trait LexicalAnalyzer {
  def tokenize(source: String): LexicalAnalysisFailed \/ Seq[LexerToken]
}