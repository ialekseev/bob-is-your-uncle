package com.ialekseev.bob.run

import java.nio.file.{Files, Paths}
import com.ialekseev.bob._
import com.ialekseev.bob.run._
import com.ialekseev.bob.exec.Executor._
import com.ialekseev.bob.exec.analyzer.Analyzer.AnalysisResult
import scala.io.{Codec, Source, StdIn}
import scala.language.reflectiveCalls
import scala.util.Try
import scala.collection.JavaConverters._
import scalaz.Scalaz._
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.concurrent.Task
import scalaz.{-\/, EitherT, \/, \/-}
import fs2.interop.scalaz._

trait IoShared {

  val defaultSourcesLocation = "bobs"
  val varsFileName = "_vars.json"
  val fileExtension = ".bob"

  def listFiles(dir: String): Task[List[String]] = {
    require(dir.nonEmpty)

    Task {
      Files.newDirectoryStream(Paths.get(dir)).iterator().asScala.map(_.toAbsolutePath.toString).toList
    }
  }

  def listSourceFiles(dir: String): Task[List[String]] = {
    for {
      files <- listFiles(dir)
      sourceFiles <- Task.now(files.filter(_.endsWith(fileExtension)))
    } yield sourceFiles
  }

  def findVarsFile(dir: String): Task[Option[String]] = {
    for {
      files <- listFiles(dir)
      varsFile <- Task.now(files.find(f => Paths.get(f).getFileName.toString == varsFileName))
    } yield varsFile
  }

  def readFile(filePath: String): Task[String] = {
    require(filePath.nonEmpty)

    fs2.io.file.readAll[Task](Paths.get(filePath), 4096).through(fs2.text.utf8Decode).through(fs2.text.lines).intersperse("\n").runFold("")(_ + _)
  }

  def updateFile(filePath: String, content: String): Task[Unit] = {
    require(filePath.nonEmpty)
    require(content.nonEmpty)

    for {
      _ <- Task(Files.deleteIfExists(Paths.get(filePath)))
      _ <- fs2.Stream.eval(Task.now(content)).through(fs2.text.utf8Encode).through(fs2.io.file.writeAll(Paths.get(filePath))).run
    } yield (): Unit
  }

  def extractVarsFromVarsFile(varsFilePath: String): Task[List[Variable[String]]] = {
    require(varsFilePath.nonEmpty)

    import org.json4s._
    import org.json4s.native.JsonMethods._
    implicit val formats = org.json4s.DefaultFormats

    for {
      text <- readFile(varsFilePath)
      vars <- Task(parse(text).extract[Map[String, String]].map(t => Variable(t._1, t._2)).toList)
    } yield vars
  }

  def extractVarsForDir(dir: String): Task[List[Variable[String]]] = {
    require(dir.nonEmpty)

    for {
      varsFile <- findVarsFile(dir)
      vars <- varsFile match {
        case Some(f) => extractVarsFromVarsFile(f)
        case None => Task.now(List.empty)
      }
    } yield vars
  }

  def extractVarsForFile(filePath: String): Task[List[Variable[String]]] = {
    require(filePath.nonEmpty)

    extractVarsForDir(Paths.get(filePath).getParent.toAbsolutePath.toString)
  }

  def readSource(sourceFilePath: String): Task[InputSource] = {
    require(sourceFilePath.nonEmpty)

    for {
      content <- readFile(sourceFilePath)
      vars <- extractVarsForFile(sourceFilePath)
    } yield InputSource(sourceFilePath, content, vars)
  }

  def readSources(dirs: List[String]): Task[List[InputSource]] = {
    require(dirs.nonEmpty)

    case class FileWithVars(filePath: String, vars: List[Variable[String]])

    def getFilesWithVars: Task[List[FileWithVars]] = {
      dirs.map(dir => {
        for {
          sourceFiles <- listSourceFiles(dir)
          vars <- extractVarsForDir(dir)
        } yield sourceFiles.map(f => FileWithVars(f, vars))
      }).sequenceU.map(_.flatten)
    }

    def mapToInputSources(files: List[FileWithVars]): Task[List[InputSource]] = {
      files.map(file => {
        readFile(file.filePath).map(l => InputSource(file.filePath, l, file.vars))
      }).sequenceU
    }

    for {
      sourceFiles <- getFilesWithVars
      sources <- mapToInputSources(sourceFiles)
    } yield sources
  }

  def updateSource(sourceFilePath: String, content: String): Task[InputSource] = {
    require(sourceFilePath.nonEmpty)
    require(content.nonEmpty)

    for {
      _ <- updateFile(sourceFilePath, content)
      source <- readSource(sourceFilePath)
    } yield source
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
