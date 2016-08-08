package com.ialekseev.bob

import com.ialekseev.bob.analyzer.{DefaultAnalyzer}
import com.ialekseev.bob.exec.{ScalaCompiler, Executor}
import scala.util.{Failure, Success, Try}
import scalaz._

object Boot extends App {

  case class Config(check: String = "")
  val parser = new scopt.OptionParser[Config]("bob") {
    opt[String]("check").action( (x, c) =>
      c.copy(check = x) ).text("check - is a path to file")
  }

  //todo: refactor this mess
  parser.parse(args, Config()) match {
    case Some(Config(check)) if check.nonEmpty => {
      val fileToCheck = scala.io.Source.fromFile(check)
      Try(fileToCheck.mkString("\n")) match {
        case Success(content) => {
          println(Console.CYAN + "  +-------------------------------+")
          println(Console.CYAN + "  | Bob: I am checking...         |")
          println(Console.CYAN + "  +-------------------------------+")
          println()

          val executor = new Executor {
            val analyzer = DefaultAnalyzer
            val scalaCompiler = new ScalaCompiler
          }
          executor.check(content) match {
            case \/-(result) => println(Console.GREEN + "   OK")
            case -\/(error) => error match {
              case LexicalAnalysisFailed(first +: _) => Console.println(Console.RED + s"   Lexical error on the positions from [${first.startOffset} to ${first.endOffset}]")
              case SyntaxAnalysisFailed(first +: _) => Console.println(Console.RED + s"   Error on the position [${first.offset}]. Message: ${first.message}")
              case SemanticAnalysisFailed(first +: _) => Console.println(Console.RED + s"   Error on the position [${first.offset}]. Message: ${first.message}")
              case CompilationFailed(first +: _) => Console.println(Console.RED +  s"   Error on the position [${first.startOffset}]. Message: ${first.message}")
            }
          }
        }
        case Failure(e) => println(Console.RED + "   Can't read the file provided. Internal error: " + e)
      }
      fileToCheck.close()
      println()
      println(Console.CYAN + "  +-------------------------------+")
    }
    case None =>
  }

}
