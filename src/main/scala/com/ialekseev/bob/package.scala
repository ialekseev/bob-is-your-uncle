package com.ialekseev

package object bob {
  case class LexicalError(startOffset: Int, endOffset: Int)
  case class SyntaxError(offset: Int, tokenIndex: Int, message: String)
  case class SemanticError(offset: Int, message: String)
  case class ExecutionError(startOffset: Int, pointOffset: Int, endOffset: Int, message: String)

  sealed trait AnalysisFailed
  case class LexicalAnalysisFailed(errors: Seq[LexicalError]) extends AnalysisFailed
  case class SyntaxAnalysisFailed(errors: Seq[SyntaxError]) extends AnalysisFailed
  case class SemanticAnalysisFailed(errors: Seq[SemanticError]) extends AnalysisFailed
  case class ExecutionAnalysisFailed(errors: Seq[ExecutionError]) extends AnalysisFailed
}
