package com.ialekseev.bob.syntax

/*
  namespace com.ialekseev.core#create
    description: "creating new"

    $header: "hello"
    $createMeUri: "http://example.com/1"

    @webhook
      get: "$baseUri/%key"
      queryString: "{'a': %a, 'b': %b}"
*/

/*[Syntax in EBNF form]

    RuleDescription ::= 'description' : stringLiteral
    Constant ::= variable : stringLiteral

    WebhookGetMethod ::= INDENT INDENT 'get' : stringLiteral NL
               INDENT INDENT 'queryString' : stringLiteral NL

    Webhook ::= '@webhook' NL
             WebhookGetMethod

    RuleImpl ::= INDENT RuleDescription NL
                {INDENT Constant NL}
                INDENT (Webhook | Poll) NL
                INDENT Process NL

    NameSpace ::= 'namespace' WS NameSpacePath # namespaceId

    NameSpacePath ::= identifier {'.' identifier}

    TopStat ::= NameSpace NL
                RuleImpl
*/


class SyntaxAnalyzer {

}
