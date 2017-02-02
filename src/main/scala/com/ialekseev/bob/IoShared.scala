package com.ialekseev.bob

import java.io.File
import com.ialekseev.bob.Models._
import com.ialekseev.bob.analyzer.Analyzer.AnalysisResult
import com.ialekseev.bob.exec.Executor._
import scala.io.{StdIn, Codec, Source}
import scala.util.Try
import scalaz.{-\/, \/-, \/, EitherT}
import scalaz.effect.IO
import scalaz.Scalaz._
import scalaz.effect.IO._

//todo: Async IO ?
trait IoShared {

  val defaultSourcesLocation = "bobs"
  val varsFileName = "_vars.json"
  val fileExtension = ".bob"

  type IoTry[T] =  EitherT[IO, List[Throwable], T]
  def IoTry[T](v: List[Throwable] \/ T): EitherT[IO, List[Throwable], T] = EitherT.eitherT[IO, List[Throwable], T](IO(v))

  def listFiles(dir: String): IoTry[List[String]] = IoTry(new java.io.File(dir).listFiles.toList.map(_.getPath).right)

  def listSourceFiles(dir: String): IoTry[List[String]] = {
    for {
      files: List[String] <- listFiles(dir)
      sourceFiles: List[String] <- IoTry(files.filter(_.endsWith(fileExtension)).right)
    } yield sourceFiles
  }

  def findVarFile(dir: String): IoTry[Option[String]] = {
    for {
      files: List[String] <- listFiles(dir)
      varsFile: Option[String] <- IoTry(files.find(_ == varsFileName).right)
    } yield varsFile
  }
  
  def readFile(filePath: String): IoTry[String] = {
    require(filePath.nonEmpty)

    def normalize(source: String): String = {
      source.replaceAll("\r\n", "\n")
    }

    EitherT.eitherT[IO, List[Throwable], String] {
      IO {
        Try {
          val bufferedSource = Source.fromFile(filePath)(Codec.UTF8)
          val content = bufferedSource.mkString
          bufferedSource.close()
          content
        }.toDisjunction.leftMap(List(_))
      }
    }.map(normalize(_))
  }

  def extractVarsFromVarsFile(varsFilePath: String): IoTry[List[(String, String)]] = {
    require(varsFilePath.nonEmpty)

    import org.json4s._, org.json4s.native.JsonMethods._
    implicit val formats = org.json4s.DefaultFormats

    for {
      text: String <- readFile(varsFilePath)
      vars: List[(String, String)] <- IoTry(Try(parse(text).extract[Map[String, String]].toList).toDisjunction.leftMap(List(_)))
    } yield vars
  }

  def extractVarsForDir(dir: String): IoTry[List[(String, String)]] = {
    require(dir.nonEmpty)

    for {
      varsFile: Option[String] <- findVarFile(dir)
      vars: List[(String, String)] <- varsFile match {
        case Some(f) => extractVarsFromVarsFile(f)
        case None => IoTry(List.empty.right)
      }
    } yield vars
  }

  def extractVarsForFile(filePath: String): IoTry[List[(String, String)]] = {
    require(filePath.nonEmpty)

    extractVarsForDir(new File(filePath).getParent)
  }

  def readSource(sourceFilePath: String): IoTry[InputSource] = {
    require(sourceFilePath.nonEmpty)

    for {
      content <- readFile(sourceFilePath)
      vars <- extractVarsForFile(sourceFilePath)
    } yield InputSource(sourceFilePath, content, vars)
  }

  def readSources(dirs: List[String]): IoTry[List[InputSource]] = {
    require(dirs.nonEmpty)

    case class FileWithVars(filePath: String, vars: List[(String, String)])

    def getFilesWithVars: IoTry[List[FileWithVars]] = {
      dirs.map(dir => {
        for {
          sourceFiles: List[String] <- listSourceFiles(dir)
          vars <- extractVarsForDir(dir)
        } yield sourceFiles.map(f => FileWithVars(f, vars))
      }).sequenceU.map(_.flatten)
    }

    def mapToInputSources(files: List[FileWithVars]): IoTry[List[InputSource]] = {
      files.map(file => {
        readFile(file.filePath).map(l => InputSource(file.filePath, l, file.vars))
      }).sequenceU
    }

    for {
      sourceFiles: List[FileWithVars] <- getFilesWithVars
      sources: List[InputSource] <- mapToInputSources(sourceFiles)
    } yield sources
  }

  def show(message: String = ""): IO[Unit] = putStrLn(message)
  def read(message: String = ""): IO[String] = IO { StdIn.readLine(message)}

  def showResult(filename: String, source: String, result: BuildFailed \/ Build): IO[Unit] = {
    require(!filename.isEmpty)
    require(!source.isEmpty)

    def showSuccess(filename: String, r: AnalysisResult): IO[Unit] = {
      for {
        _ <- show()
        _ <- showFileName(filename)
        _ <- show(Console.WHITE + "namespace: " + r.namespace + Console.RESET)
        _ <- show(Console.WHITE + "description: "+ r.description + Console.RESET)
        _ <- show(Console.WHITE + "result: " + Console.GREEN + s"OK" + Console.RESET)
        _ <- show()
      } yield ()
    }

    result match {
      case \/-(built) => showSuccess(filename, built.analysisResult)
      case -\/(error) => error match {
        case LexicalAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, "Unexpected token or block")
        case SyntaxAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
        case SemanticAnalysisFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
        case CompilationFailed(first +: _) => showError(filename, source, first.startOffset, first.endOffset, first.message)
      }
    }
  }

  def showError(filename: String, source: String, startOffset: Int, endOffset: Int, message: String): IO[Unit] = {
    require(!filename.isEmpty)
    require(!source.isEmpty)
    require(startOffset >= 0)
    require(endOffset >= 0)

    def showErrorContext(source: String, startOffset: Int, endOffset: Int): IO[Unit] = {
      val before = source.substring(0, startOffset)
      val error = {
        val err = source.substring(startOffset, endOffset + 1)
        if (err.matches("\\s+")) "_" * err.length
        else err
      }
      val after = if (endOffset + 1 < source.length - 1) some(source.substring(endOffset + 1)) else none

      val context = (before, error, after)
      for {
        _ <- show(Console.RED + "[" + Console.RESET)
        _ <- show(Console.WHITE + context._1 + Console.RED + context._2 + Console.WHITE + context._3.getOrElse("") + Console.RESET)
        _ <- show(Console.RED + "]" + Console.RESET)
      } yield ()
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

    for {
      _ <- show()
      _ <- showFileName(filename)
      _ <- show(Console.RED + s"Error position: ${errorCoordinate(source, startOffset)}" + Console.RESET)
      _ <- show(Console.RED + s"Message: $message" + Console.RESET)
      _ <- showErrorContext(source, startOffset, endOffset)
      _ <- show()
    } yield ()
  }

  def showError(message: String, errors: Throwable*): IO[Unit] = {
    errors.toList.map(e => showError(s"$message. Internal error: $e")).sequenceU.map(_ => ())
  }

  def showError(message: String): IO[Unit] = {
    require(message.nonEmpty)

    for {
      _ <- show()
      _ <- show(Console.RED + message + Console.RESET)
      _ <- show()
    } yield ()
  }

  def showFileName(filename: String): IO[Unit] = {
    require(filename.nonEmpty)

    show(Console.CYAN + "[" + filename + "]" + Console.RESET)
  }
}
