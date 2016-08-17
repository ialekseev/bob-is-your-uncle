package com.ialekseev

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
}
