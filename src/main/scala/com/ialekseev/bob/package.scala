package com.ialekseev

import org.json4s.JsonAST.JValue

package object bob {
  sealed trait BuildError {
    val startOffset: Int
    val endOffset: Int
    val message: String
  }
  case class LexicalError(startOffset: Int, endOffset: Int, message: String = "") extends BuildError
  case class SyntaxError(startOffset: Int, endOffset: Int, tokenIndex: Int, message: String) extends BuildError
  case class SemanticError(startOffset: Int, endOffset: Int, message: String) extends BuildError
  case class CompilationError(startOffset: Int, pointOffset: Int, endOffset: Int, message: String) extends BuildError

  sealed trait BuildFailed {
    val errors: List[BuildError]
    val stage: String
  }
  case class LexicalAnalysisFailed(errors: List[LexicalError]) extends BuildFailed { val stage = "lexical"}
  case class SyntaxAnalysisFailed(errors: List[SyntaxError]) extends BuildFailed { val stage = "syntax"}
  case class SemanticAnalysisFailed(errors: List[SemanticError]) extends BuildFailed { val stage = "semantic"}
  case class CompilationFailed(errors: List[CompilationError]) extends BuildFailed { val stage = "compiler"}

  case class HttpRequest(uri: Option[String], method: HttpMethod.Value, headers: Map[String, String], queryString: Map[String, String], body: Option[Body]){
    require(uri.isEmpty || uri.isDefined && uri.get.nonEmpty, "Some(uri) can't be empty!")
  }

  object HttpMethod extends Enumeration {
    val GET, POST, PUT, DELETE = Value
  }

  sealed trait Body
  case class StringLiteralBody(text: String) extends Body
  case class DictionaryBody(dic: Map[String, String]) extends Body
  case class JsonBody(json: JValue) extends Body

  case class Variable[+T](name: String, value: T)
}
