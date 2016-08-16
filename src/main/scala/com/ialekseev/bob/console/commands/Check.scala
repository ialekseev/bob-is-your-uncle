package com.ialekseev.bob.console.commands

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
    val fileToCheck = scala.io.Source.fromFile(path)(Codec.UTF8)
    Try(normalizeSource(fileToCheck.mkString)) match {
      case Success(c) => {
        executor.check(c) match {
          case \/-(result) => showSuccess("OK")
          case -\/(error) => error match {
            case LexicalAnalysisFailed(first +: _) => showError(c, first.startOffset, first.endOffset)
            case SyntaxAnalysisFailed(first +: _) => showError(c, first.offset, first.message)
            case SemanticAnalysisFailed(first +: _) => showError(c, first.offset, first.message)
            case CompilationFailed(first +: _) => showError(c, first.startOffset, first.message)
          }
        }
      }
      case Failure(e) => showError("Can't read the file provided", e)
    }
    fileToCheck.close()
  }

  def showTitleMessage(message: String) = {
    println()
    println(Console.CYAN + s"  | $message |")
    println()
  }

  def showSuccess(message: String) = {
    println(Console.GREEN + s"$message" + Console.RESET)
    println()
  }

  def showErrorContext(source: String, startOffset: Int, endOffset: Int) = {
    val before = source.substring(0, startOffset)
    val error = {
      val err = source.substring(startOffset, endOffset + 1)
      if (err.head == ' ') err.updated(0, '_') else err
      //todo: underline the whole indent line
    }
    val after = if (endOffset + 1 < source.length - 1) some(source.substring(endOffset + 1)) else none

    val context = (before, error, after)
    println(Console.RED + "[" + Console.RESET)
    println(context._1 + Console.RED + context._2 + Console.RESET + context._3.getOrElse(""))
    println(Console.RED + "]" + Console.RESET)
  }


  def showError(source: String, startOffset: Int, endOffset: Int) = {
    println()
    println(Console.RED + s"Unexpected token on positions: from ${errorCoordinate(source, startOffset)} to ${errorCoordinate(source, endOffset)}" + Console.RESET)
    showErrorContext(source, startOffset, endOffset)
    println()
  }

  def showError(source: String, offset: Int, message: String) = {
    println()
    println(Console.RED + s"Error position: ${errorCoordinate(source, offset)}")
    println(s"Message: $message" + Console.RESET)
    showErrorContext(source, offset, offset)
    println()
  }

  def showError(message: String, e: Throwable) = {
    println(Console.RED + s"  $message. Internal error: $e" + Console.RESET)
  }
}
