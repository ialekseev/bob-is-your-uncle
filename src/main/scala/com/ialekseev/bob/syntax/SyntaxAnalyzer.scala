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

    NamespacePathPart ::= '.' identifier
    NamespacePathParts ::= {NamespacePathPart}
    NamespacePath ::= identifier NamespacePathPart
    Namespace ::= 'namespace' NamespacePath # identifier

    Description ::= 'description' : stringLiteral

    Constant ::= variable : stringLiteral
    Constants ::= {Constant}

    WebhookUriSetting ::= 'uri' : stringLiteral

    WebhookSpecificSetting ::= 'method' : stringLiteral |
                               'queryString' : stringLiteral

    WebhookSpecificSettings ::= {WebhookSpecificSetting}

    WebhookSettings ::= WebhookUriSetting
                        WebhookSpecificSettings

    Webhook ::= '@webhook'
                  WebhookSettings

    RuleImpl ::= Description
                 Constants
                 Webhook

    TopStat ::= Namespace
                 RuleImpl
*/

trait SyntaxAnalyzer
