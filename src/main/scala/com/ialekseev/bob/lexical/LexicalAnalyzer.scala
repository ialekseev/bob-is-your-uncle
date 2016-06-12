package com.ialekseev.bob.lexical

import com.ialekseev.bob.Token

/*[Lexical Syntax in EBNF form]
      upper ::=  'A' |...| 'Z'
      lower ::=  'a' |...| 'z'
      letter ::= upper | lower
      digit ::=  '0' |...| '9'
      id ::= letter | digit
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
  def tokenize(input: String): Either[List[LexicalAnalysisError], List[Token]]
}

case class LexicalAnalysisError(startOffset: Int, endOffset: Int)