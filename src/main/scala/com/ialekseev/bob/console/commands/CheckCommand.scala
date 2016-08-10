package com.ialekseev.bob.console.commands

import com.ialekseev.bob.analyzer.DefaultAnalyzer
import com.ialekseev.bob.console.{BobConsole, Command}
import com.ialekseev.bob.exec.{Executor, ScalaCompiler}
import com.ialekseev.bob.{CompilationFailed, LexicalAnalysisFailed, SemanticAnalysisFailed, SyntaxAnalysisFailed}
import scala.io.Codec
import scala.util.{Failure, Success, Try}
import scalaz._

trait CheckCommand {
  this: Command with BobConsole =>

  def checkCommand(path: String) = {
    val fileToCheck = scala.io.Source.fromFile(path)(Codec.UTF8)
    Try(normalizeSource(fileToCheck.mkString)) match {
      case Success(c) => {
        showTitleMessage("Bob: I am checking...")

        val executor = new Executor {
          val analyzer = DefaultAnalyzer
          val scalaCompiler = new ScalaCompiler
        }
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
}
