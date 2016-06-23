package com.ialekseev.bob.syntax

/*[Example]
  namespace com.ialekseev.core#create
    description: "{description}"

    $var1: "{var1}"
    $var2: "{var2}"

    @webhook
      method: "{method}"
      uri: "{binding}"
      queryString: "{binding}"
*/

/*[Syntax in EBNF form]

    NamespacePath ::= identifier {'.' identifier}
    Namespace ::= 'namespace' WS NamespacePath # identifier

    Description ::= 'description' : stringLiteral

    Constants ::= {variable : stringLiteral}

    WebhookImpl ::= 'method' : stringLiteral
                    'uri' : stringLiteral
                    'queryString' : stringLiteral

    Webhook ::= '@webhook'
                  WebhookImpl

    RuleImpl ::= Description
                 Constants
                 Webhook

    TopStat ::= Namespace
                 RuleImpl
*/


class SyntaxAnalyzer {

}
