package com.ialekseev

import com.ialekseev.bob.HttpMethod.HttpMethod
import org.json4s.JsonAST.JValue

package object bob {
  case class LexicalError(startOffset: Int, endOffset: Int)
  case class SyntaxError(startOffset: Int, endOffset: Int, tokenIndex: Int, message: String)
  case class SemanticError(startOffset: Int, endOffset: Int, message: String)
  case class CompilationError(startOffset: Int, pointOffset: Int, endOffset: Int, message: String)

  sealed trait StageFailed
  case class LexicalAnalysisFailed(errors: Seq[LexicalError]) extends StageFailed
  case class SyntaxAnalysisFailed(errors: Seq[SyntaxError]) extends StageFailed
  case class SemanticAnalysisFailed(errors: Seq[SemanticError]) extends StageFailed
  case class CompilationFailed(errors: Seq[CompilationError]) extends StageFailed

  case class HttpRequest(uri: String, method: HttpMethod, headers: Map[String, String], queryString: Map[String, String], body: Option[Body])
  object HttpMethod extends Enumeration {
    type HttpMethod = Value
    val GET, POST, PUT, DELETE = Value
  }

  sealed trait Body
  case class StringLiteralBody(text: String) extends Body
  case class DictionaryBody(dic: Map[String, String]) extends Body
  case class JsonBody(j: JValue) extends Body
}
