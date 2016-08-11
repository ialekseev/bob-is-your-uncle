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
        showTitleMessage("Bob: I am checking...")

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
    println(Console.GREEN + s"  $message" + Console.RESET)
  }

  def showError(source: String, startOffset: Int, endOffset: Int) = {
    println(Console.RED + s"  Error positions: from $startOffset} to $endOffset}" + Console.RESET)
  }

  def showError(source: String, offset: Int, message: String) = {
    def showErrorPosition() = {
      println(Console.RED + s"  Error position: ${errorCoordinate(source, offset)}. Message: $message" + Console.RESET)
    }

    def showErrorContext() = {
      val after = if (offset + 1 < source.length - 1) some(source.substring(offset + 1)) else none
      val context = (source.substring(0, offset), source(offset), after)
      println(Console.RED + "  [" + Console.RESET)
      println(context._1 + Console.RED + context._2 + Console.RESET + context._3.getOrElse(""))
      println(Console.RED + "  ]" + Console.RESET)
    }

    println()
    showErrorPosition()
    showErrorContext()
    println()
  }

  def showError(message: String, e: Throwable) = {
    println(Console.RED + s"  $message. Internal error: $e" + Console.RESET)
  }
}
