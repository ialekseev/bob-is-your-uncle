package com.ialekseev.bob.console.commands

import com.ialekseev.bob.analyzer.Analyzer.AnalysisResult
import com.ialekseev.bob.analyzer.DefaultAnalyzer
import com.ialekseev.bob.console.{Command}
import com.ialekseev.bob.exec.{Executor, ScalaCompiler}
import com.ialekseev.bob.{CompilationFailed, LexicalAnalysisFailed, SemanticAnalysisFailed, SyntaxAnalysisFailed}
import scala.io.Codec
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._

trait Check {
  this: Command =>

  val executor = new Executor {
    val analyzer = DefaultAnalyzer
    val scalaCompiler = new ScalaCompiler
  }

  def checkCommand(path: String) = {
    Try {
      val fileToCheck = scala.io.Source.fromFile(path)(Codec.UTF8)
      val source = normalizeSource(fileToCheck.mkString)
      fileToCheck.close()
      source
    } match {
      case Success(c) => {
        executor.check(c) match {
          case \/-(result) => showSuccess(result)
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

  def showSuccess(result: AnalysisResult) = {
    println()
    println(Console.WHITE + "namespace: " + result.namespace.path + "#" + result.namespace.name + Console.RESET)
    println(Console.WHITE + "description: "+ result.description + Console.RESET)
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
