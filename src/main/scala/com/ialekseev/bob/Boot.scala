package com.ialekseev.bob

import com.ialekseev.bob.analyzer.{DefaultAnalyzer}
import com.ialekseev.bob.exec.{ScalaCompiler, Executor}
import scala.io.Codec
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
      val fileToCheck = scala.io.Source.fromFile(check)(Codec.UTF8)
      Try(normalizeSource(fileToCheck.mkString)) match {
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
              case LexicalAnalysisFailed(first +: _) => Console.println(Console.RED + s"   Lexical error positions: from ${errorCoordinate(content, first.startOffset)} to ${errorCoordinate(content, first.endOffset)}")
              case SyntaxAnalysisFailed(first +: _) => Console.println(Console.RED + s"   Error position: ${errorCoordinate(content, first.offset)}. Message: ${first.message}")
              case SemanticAnalysisFailed(first +: _) => Console.println(Console.RED + s"   Error position: ${errorCoordinate(content, first.offset)}. Message: ${first.message}")
              case CompilationFailed(first +: _) => Console.println(Console.RED +  s"   Error position: ${errorCoordinate(content, first.startOffset)}. Message: ${first.message}")
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


  private def normalizeSource(source: String): String = {
    source.replaceAll("\r\n", "\n")
  }

  private def errorCoordinate(source: String, offset: Int): (Int, Int) = {
    require(offset >= 0)

    if (source.isEmpty || offset == 0) (0, 0)
    else {
      val beforeOffset = source.take(offset)
      val nlIndex = beforeOffset.reverse.indexWhere(_ == '\n')

      val column = if (nlIndex > 0) nlIndex else 0
      val line = beforeOffset.count(_ == '\n')
      (line, column)
    }
  }
}
