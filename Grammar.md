namespace com.ialekseev.core#create
	description: "creating new"
		 
	$header: "hello"
	$createMeUri: "http://example.com/1"
	
	@webhook		
		get: "$baseUri/%key"
		queryString: "{'a': %a, 'b': %b}"																					
							
################################################################


[Lexical Syntax]
************************************
upper ::=  ‘A’ |…| ‘Z’
lower ::=  ‘a’ |…| ‘z’
letter ::= upper | lower 
digit ::=  ‘0’ |…| ‘9’
idSymbol ::= letter | digit
id ::= idSymbol {idSymbol}
varId ::= ‘$’ id

stringLiteral = "string literal"
newLine ::= ‘\n’
whiteSpace ::= ‘ ’

NL ::= {whiteSpace} newLine {newLine}
WS ::= whiteSpace {whiteSpace}
COLON ::= {whiteSpace} : {whiteSpace}
INDENT ::= "indent"

[Context-free syntax]
***************************************
RuleDescription ::= 'description' COLON stringLiteral
Constant ::= varId COLON stringLiteral

WebhookGetMethod ::= INDENT INDENT 'get' COLON stringLiteral NL
					 INDENT INDENT 'queryString' COLON stringLiteral NL		

Webhook ::= '@webhook' NL
				 WebhookGetMethod	

RuleImpl ::= INDENT RuleDescription NL
			{INDENT Constant NL}
			INDENT (Webhook | Poll) NL
			INDENT Process NL					

NameSpace ::= 'namespace' WS NameSpacePath # namespaceId

NameSpacePath ::= id {‘.’ id}

TopStat ::= NameSpace NL
			RuleImpl

			
			

			
			

			
			


			
			
			

			
			
			

			
			
################################################################
namespace com.ialekseev.core#create
	description: "creating new"
		 
	$header: {"SECRET" : "XXX"}
	$createUri: "http://example.com/1"
	
	@webhook		
		get: $baseUri/%key
		queryString: "{"a": %a, "b": %b}"
		filter: %a equals "name"
			
	@poll		
		get: http://example.com, 
		queryString: {"c": "b"}
		period: "2h"
		produce:
			$got: http(get: $createUri, queryString: {"c": $a}, header: $header)			
										
								
	@process			
		if ($a equals "1" and $b contains "2")
			http(get: $createUri, queryString: {"c": $a}, header: $header)
		else
			http(get: $createUri, params: {"c": $a})
							
################################################################			