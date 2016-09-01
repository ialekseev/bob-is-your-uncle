package com.ialekseev.bob.run.commands

import com.ialekseev.bob.analyzer.Analyzer.AnalysisResult
import com.ialekseev.bob.run.{Command}
import com.ialekseev.bob.{CompilationFailed, LexicalAnalysisFailed, SemanticAnalysisFailed, SyntaxAnalysisFailed}
import scala.io.Codec
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._

trait Check {
  this: Command =>

  def checkCommand(path: String) = {
    Try {
      val fileToCheck = scala.io.Source.fromFile(path)(Codec.UTF8)
      val source = normalizeSource(fileToCheck.mkString)
      fileToCheck.close()
      source
    } match {
      case Success(c) => {
        exec.build(c).unsafePerformSync match {
          case \/-(built) => showSuccess(built.analysisResult)
          case -\/(error) => error match {
            case LexicalAnalysisFailed(first +: _) => showError(c, first.startOffset, first.endOffset, "Unexpected token")
            case SyntaxAnalysisFailed(first +: _) => showError(c, first.startOffset, first.endOffset, first.message)
            case SemanticAnalysisFailed(first +: _) => showError(c, first.startOffset, first.endOffset, first.message)
            case CompilationFailed(first +: _) => showError(c, first.startOffset, first.endOffset, first.message)
          }
        }
      }
      case Failure(e) => showError("Can't read the file provided", e)
    }
  }

  def normalizeSource(source: String): String = {
    source.replaceAll("\r\n", "\n")
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

  def showSuccess(r: AnalysisResult) = {
    println()
    println(Console.WHITE + "namespace: " + r.namespace.path + "#" + r.namespace.name + Console.RESET)
    println(Console.WHITE + "description: "+ r.description + Console.RESET)
    println(Console.WHITE + "result: " + Console.GREEN + s"OK" + Console.RESET)
    println()
  }

  def showError(source: String, startOffset: Int, endOffset: Int, message: String) = {
    def showErrorContext(source: String, startOffset: Int, endOffset: Int) = {
      val before = source.substring(0, startOffset)
      val error = {
        val err = source.substring(startOffset, endOffset + 1)
        if (err.matches("\\s+")) "_" * err.length
        else err
      }
      val after = if (endOffset + 1 < source.length - 1) some(source.substring(endOffset + 1)) else none

      val context = (before, error, after)
      println(Console.RED + "[" + Console.RESET)
      println(Console.WHITE + context._1 + Console.RED + context._2 + Console.WHITE + context._3.getOrElse("") + Console.RESET)
      println(Console.RED + "]" + Console.RESET)
    }

    println()
    println(Console.RED + s"Error position: ${errorCoordinate(source, startOffset)}" + Console.RESET)
    println(Console.RED + s"Message: $message" + Console.RESET)
    showErrorContext(source, startOffset, endOffset)
    println()
  }

  def showError(message: String, e: Throwable) = {
    println()
    println(Console.RED + s"$message. Internal error: $e" + Console.RESET)
    println()
  }
}
