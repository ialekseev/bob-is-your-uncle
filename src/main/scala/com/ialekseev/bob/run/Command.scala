package com.ialekseev.bob.run

import com.ialekseev.bob.analyzer.Analyzer.AnalysisResult
import com.ialekseev.bob.{CompilationFailed, SemanticAnalysisFailed, SyntaxAnalysisFailed, LexicalAnalysisFailed}
import com.ialekseev.bob.analyzer.DefaultAnalyzer
import com.ialekseev.bob.exec.Executor.{BuildFailed, Build}
import com.ialekseev.bob.exec.{ScalaCompiler, Executor}
import scala.util.Try
import scalaz.Scalaz._
import scalaz.{-\/, \/-, \/}

trait Command {
  val exec = new Executor {
    val analyzer = DefaultAnalyzer
    val scalaCompiler = new ScalaCompiler
  }

  def show(message: String = ""): Unit
  def read(message: String = ""): String
  def readSource(filename: String): Try[String]
  def readSources(dir: String): Try[List[(String, String)]]

  def showResult(filename: String, source: String, result: BuildFailed \/ Build) = {
    require(!filename.isEmpty)
    require(!source.isEmpty)

    def showSuccess(filename: String, r: AnalysisResult) = {
      show()
      showFileName(filename)
      show(Console.WHITE + "namespace: " + r.namespace.path + "#" + r.namespace.name + Console.RESET)
      show(Console.WHITE + "description: "+ r.description + Console.RESET)
      show(Console.WHITE + "result: " + Console.GREEN + s"OK" + Console.RESET)
      show()
    }

    result match {
      case \/-(built) => showSuccess(filename, built.analysisResult)
      case -\/(error) => error match {
        case LexicalAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, "Unexpected token")
        case SyntaxAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
        case SemanticAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
        case CompilationFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
      }
    }
  }

  def showError(filename: String, source: String, startOffset: Int, endOffset: Int, message: String) = {
    require(!filename.isEmpty)
    require(!source.isEmpty)
    require(startOffset >= 0)
    require(endOffset >= 0)

    def showErrorContext(source: String, startOffset: Int, endOffset: Int) = {
      val before = source.substring(0, startOffset)
      val error = {
        val err = source.substring(startOffset, endOffset + 1)
        if (err.matches("\\s+")) "_" * err.length
        else err
      }
      val after = if (endOffset + 1 < source.length - 1) some(source.substring(endOffset + 1)) else none

      val context = (before, error, after)
      show(Console.RED + "[" + Console.RESET)
      show(Console.WHITE + context._1 + Console.RED + context._2 + Console.WHITE + context._3.getOrElse("") + Console.RESET)
      show(Console.RED + "]" + Console.RESET)
    }

    def errorCoordinate(source: String, offset: Int): (Int, Int) = {
      require(offset >= 0)

      if (source.isEmpty || offset == 0) (1, 1)
      else {
        val beforeOffset = source.take(offset)
        val nlIndex = beforeOffset.reverse.indexWhere(_ == '\n')

        val column = if (nlIndex >= 0) nlIndex + 1 else offset + 1
        val line = beforeOffset.count(_ == '\n') + 1
        (line, column)
      }
    }

    show()
    showFileName(filename)
    show(Console.RED + s"Error position: ${errorCoordinate(source, startOffset)}" + Console.RESET)
    show(Console.RED + s"Message: $message" + Console.RESET)
    showErrorContext(source, startOffset, endOffset)
    show()
  }

  def showError(message: String, e: Throwable) = {
    show()
    show(Console.RED + s"$message. Internal error: $e" + Console.RESET)
    show()
  }

  def showFileName(filename: String) = {
    require(!filename.isEmpty)

    show(Console.CYAN + "[" + filename + "]" + Console.RESET)
  }
}
